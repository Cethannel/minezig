const std = @import("std");
const Build = std.Build;

const textures = @import("src/textures.zig");

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
    var controllerSupport = !(b.option(
        bool,
        "dissableController",
        "Disstable controller support",
    ) orelse false);

    const dep_sokol = b.dependency("sokol", .{
        .target = target,
        .optimize = optimize,
        .with_sokol_imgui = true,
    });

    const zigimg_dependency = b.dependency("zigimg", .{
        .target = target,
        .optimize = optimize,
    });

    const zlm = b.dependency("zlm", .{});

    const dep_cimgui = b.dependency("cimgui", .{
        .target = target,
        .optimize = optimize,
    });

    // inject the cimgui header search path into the sokol C library compile step
    dep_sokol.artifact("sokol_clib").addIncludePath(dep_cimgui.path("src"));

    const exe = b.addExecutable(.{
        .name = "sokol-test",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    if (target.result.os.tag == .windows) {
        controllerSupport = false;
    }

    addControllerSupport(b, target, exe, controllerSupport);

    const imports = [_]struct {
        name: []const u8,
        dep: *Build.Dependency,
    }{
        .{
            .name = "sokol",
            .dep = dep_sokol,
        },
        .{
            .name = "zigimg",
            .dep = zigimg_dependency,
        },
        .{
            .name = "cimgui",
            .dep = dep_cimgui,
        },
        .{
            .name = "zlm",
            .dep = zlm,
        },
    };

    for (imports) |import| {
        exe.root_module.addImport(import.name, import.dep.module(import.name));
    }

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
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    addControllerSupport(b, target, exe_unit_tests, controllerSupport);

    for (imports) |import| {
        exe_unit_tests.root_module.addImport(import.name, import.dep.module(import.name));
    }

    exe_unit_tests.root_module.addOptions("config", options);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const shaderState = buildShaders(b, target);

    exe.step.dependOn(shaderState);
}

fn buildShaders(b: *Build, target: Build.ResolvedTarget) *Build.Step {
    const sokol_tools_bin_dir = "tools/";

    const shaders_dir = "src/shaders/";
    const shaders = .{
        "triangle.glsl",
        "cube.glsl",
        "selector.glsl",
    };

    const shdc = "sokol-shdc";

    const shdc_path = sokol_tools_bin_dir ++ shdc;
    const shdc_step = b.step("shaders", "Compile shaders (needs ../sokol-tools-bin)");
    const glsl = if (target.result.isDarwin()) "glsl410" else "glsl430";
    const slang = glsl ++ ":metal_macos:hlsl5:glsl300es:wgsl";
    inline for (shaders) |shader| {
        const cmd = b.addSystemCommand(&.{
            shdc_path,
            "-i",
            shaders_dir ++ shader,
            "-o",
            shaders_dir ++ shader ++ ".zig",
            "-l",
            slang,
            "-f",
            "sokol_zig",
            "--reflection",
        });
        shdc_step.dependOn(&cmd.step);
    }

    return shdc_step;
}

fn addControllerSupport(
    b: *Build,
    target: Build.ResolvedTarget,
    compile: *Build.Step.Compile,
    controllerSupport: bool,
) void {
    if (controllerSupport) {
        compile.addIncludePath(b.path("externalDeps/libstem_gamepad"));

        compile.addCSourceFiles(.{
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

        compile.addCSourceFiles(.{
            .files = osFile,
            .root = b.path("externalDeps/libstem_gamepad"),
        });
    }
}
