const std = @import("std");
const Build = std.Build;

const textures = @import("src/textures.zig");

pub const shdc = @import("shdc");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const chunkgen = b.option(bool, "chunkGenLog", "Log generating chunks") orelse false;
    var controllerSupport = (b.option(
        bool,
        "dissableController",
        "Disstable controller support",
    ) orelse false);

    const std_args = .{
        .target = target,
        .optimize = optimize,
    };

    const dep_sokol = b.dependency("sokol", .{
        .target = target,
        .optimize = optimize,
        .with_sokol_imgui = true,
    });

    const zignal_dependency = b.dependency("zignal", std_args);

    const zlm = b.dependency("zlm", .{});

    const dep_cimgui = b.dependency("cimgui", std_args);

    const zclay = b.dependency("zclay", std_args);

    const uuid = b.dependency("uuid", std_args);

    const ziglangSet = b.dependency("ziglangSet", std_args);

    const vulkan = b.dependency("vulkan", .{
        .target = target,
        .optimize = std.builtin.OptimizeMode.ReleaseSafe,
        .registry = b.dependency("vulkan_headers", .{}).path("registry/vk.xml"),
    });

    const glfw = b.dependency("zglfw", std_args);

    const sdl_dep = b.dependency("sdl", .{
        .target = target,
        .optimize = optimize,
    });
    const sdl_lib = sdl_dep.artifact("SDL3");

    // inject the cimgui header search path into the sokol C library compile step
    dep_sokol.artifact("sokol_clib").addIncludePath(dep_cimgui.path("src"));

    const imports: []const std.Build.Module.Import = &.{
        .{ .name = "sokol", .module = dep_sokol.module("sokol") },
        .{ .name = "zignal", .module = zignal_dependency.module("zignal") },
        .{ .name = "cimgui", .module = dep_cimgui.module("cimgui") },
        .{ .name = "zlm", .module = zlm.module("zlm") },
        .{ .name = "zclay", .module = zclay.module("zclay") },
        .{ .name = "uuid", .module = uuid.module("uuid") },
        .{ .name = "ziglangSet", .module = ziglangSet.module("ziglangSet") },
        .{ .name = "vulkan", .module = vulkan.module("vulkan-zig") },
        .{ .name = "glfw", .module = glfw.module("glfw") },
    };

    const vert_cmd = b.addSystemCommand(&.{
        "glslc",
        "--target-env=vulkan1.2",
        "-o",
    });
    const vert_spv = vert_cmd.addOutputFileArg("vert.spv");
    vert_cmd.addFileArg(b.path("src/shaders/cube.vert"));

    const frag_cmd = b.addSystemCommand(&.{
        "glslc",
        "--target-env=vulkan1.2",
        "-o",
    });
    const frag_spv = frag_cmd.addOutputFileArg("frag.spv");
    frag_cmd.addFileArg(b.path("src/shaders/cube.frag"));

    const exe_mod = b.addModule("minezig", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = imports,
    });

    exe_mod.linkLibrary(sdl_lib);

    exe_mod.linkSystemLibrary("glfw", .{ .needed = true, .preferred_link_mode = .static });

    exe_mod.addAnonymousImport(
        "vertex_shader",
        .{ .root_source_file = vert_spv },
    );

    exe_mod.addAnonymousImport(
        "fragment_shader",
        .{ .root_source_file = frag_spv },
    );

    const exe = b.addExecutable(.{
        .name = "minezig",
        .root_module = exe_mod,
    });

    if (target.result.os.tag == .windows) {
        controllerSupport = false;
    }

    addControllerSupport(b, target, exe, controllerSupport);
    //addWasmSupport(b, target, exe);

    const options = b.addOptions();
    options.addOption(bool, "chunkGenLog", chunkgen);
    options.addOption(bool, "controllerSupport", controllerSupport);

    exe.root_module.addOptions("config", options);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    const install_docs = b.addInstallDirectory(.{
        .source_dir = exe.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Install docs into zig-out/docs");
    docs_step.dependOn(&install_docs.step);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });
    addControllerSupport(b, target, exe_unit_tests, controllerSupport);

    exe_unit_tests.root_module.addOptions("config", options);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const shaderState = try buildShaders(
        b,
        target,
        exe.root_module,
        imports,
    );

    exe.step.dependOn(shaderState);

    const mangohud = b.addSystemCommand(&.{"mangohud"});
    mangohud.addFileArg(exe.getEmittedBin());
    mangohud.step.dependOn(&exe.step);

    const mangohud_step = b.step("mangohud", "Run exe with mangohud");
    mangohud_step.dependOn(&mangohud.step);
}

fn buildShaders(
    b: *Build,
    target: Build.ResolvedTarget,
    root_module: *Build.Module,
    imports: []const std.Build.Module.Import,
) !*Build.Step {
    const shaders_dir = "src/shaders/";
    const shaders = .{
        "triangle.glsl",
        "cube.glsl",
        "selector.glsl",
        "crosshair.glsl",
    };

    const shdc_step = b.step("shaders", "Compile shaders (needs ../sokol-tools-bin)");
    inline for (shaders) |shader| {
        const in_path: []const u8 = shaders_dir ++ shader;
        const out_path: []const u8 = shaders_dir ++ shader ++ ".zig";
        const create_shdc = try shdc.createSourceFile(b, .{
            .shdc_dep = b.dependency("shdc", .{}),
            .input = in_path,
            .output = out_path,
            .slang = .{
                .metal_macos = true,
                .hlsl5 = true,
                .wgsl = true,
                .glsl430 = true,
            },
            .reflection = true,
        });
        shdc_step.dependOn(create_shdc);
        const shader_module = b.createModule(.{
            .root_source_file = b.path(out_path),
            .target = target,
            .imports = imports,
        });
        root_module.addImport(shader, shader_module);
    }

    return shdc_step;
}

fn addWasmSupport(
    b: *Build,
    target: Build.ResolvedTarget,
    compile: *Build.Step.Compile,
) void {
    _ = target;
    compile.addIncludePath(b.path("externalDeps/wasmtime/include"));

    compile.addObjectFile(b.path("externalDeps/wasmtime/lib64/libwasmtime.a"));
}

fn addControllerSupport(
    b: *Build,
    target: Build.ResolvedTarget,
    compile: *Build.Step.Compile,
    controllerSupport: bool,
) void {
    if (controllerSupport) {
        compile.addIncludePath(b.path("externalDeps/libstem_gamepad"));

        compile.root_module.addCSourceFiles(.{
            .files = &.{
                "Gamepad_private.c",
            },
            .root = b.path("externalDeps/libstem_gamepad"),
        });

        const osFile: []const []const u8 = switch (target.result.os.tag) {
            .linux => &.{
                "Gamepad_linux.c",
            },
            .windows => windows: {
                break :windows &.{
                    "Gamepad_windows_dinput.c",
                    "Gamepad_windows_mm.c",
                };
            },
            .macos => &.{
                "Gamepad_macosx.c",
            },
            else => @panic("Unkown os"),
        };

        compile.root_module.addCSourceFiles(.{
            .files = osFile,
            .root = b.path("externalDeps/libstem_gamepad"),
        });
    }
}

const Import = struct {
    name: []const u8,
    dep: *Build.Dependency,
};
