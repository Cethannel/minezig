const std = @import("std");
const vk = @import("vulkan");
const glfw = @import("glfw");
const zlm = @import("zlm");
const zignal = @import("zignal");

const main = @import("main.zig");

const blocks = @import("blocks.zig");
const textures = @import("textures.zig");
const util = @import("utils.zig");
const workerThread = @import("workerThread.zig");
const chunks = @import("chunks.zig");
const zset = @import("ziglangSet");

const c = @cImport({
    @cInclude("GLFW/glfw3.h");
});

const IZlm = zlm.SpecializeOn(i64);
pub const IVec3 = IZlm.Vec3;

const builtin = @import("builtin");

pub const BaseWrapper = vk.BaseWrapper;
pub const InstanceWrapper = vk.InstanceWrapper;
pub const DeviceWrapper = vk.DeviceWrapper;

pub const Device = vk.DeviceProxy;

const state = &main.state;

allocator: std.mem.Allocator,
window: ?*glfw.Window = null,
instance: vk.Instance = .null_handle,
debugMessenger: vk.DebugUtilsMessengerEXT = .null_handle,

physical_device: vk.PhysicalDevice = .null_handle,
device: vk.Device = .null_handle,
graphics_queue: vk.Queue = .null_handle,
present_queue: vk.Queue = .null_handle,
surface: vk.SurfaceKHR = .null_handle,

swapchain: vk.SwapchainKHR = .null_handle,
swapchain_images: std.ArrayList(vk.Image) = .empty,
swapchain_image_format: vk.Format = .undefined,
swapchain_extent: vk.Extent2D = .{ .height = undefined, .width = undefined },
swapchain_image_views: std.ArrayList(vk.ImageView) = .empty,
swapchain_framebuffers: std.ArrayList(vk.Framebuffer) = .empty,

render_pass: vk.RenderPass = .null_handle,
descriptor_set_layout: vk.DescriptorSetLayout = .null_handle,
chunk_descriptor_set_layout: vk.DescriptorSetLayout = .null_handle,
pipeline_layout: vk.PipelineLayout = .null_handle,
graphics_pipeline: vk.Pipeline = .null_handle,

command_pool: vk.CommandPool = .null_handle,
descriptor_pool: vk.DescriptorPool = .null_handle,
chunks_descriptor_pool: vk.DescriptorPool = .null_handle,
descriptor_sets: [MAX_FRAMES_IN_FLIGHT]vk.DescriptorSet = @splat(.null_handle),
command_buffers: [MAX_FRAMES_IN_FLIGHT]vk.CommandBuffer = @splat(.null_handle),

vertex_buffer: vk.Buffer = .null_handle,
vertex_buffer_memory: vk.DeviceMemory = .null_handle,
index_buffer: vk.Buffer = .null_handle,
index_buffer_memory: vk.DeviceMemory = .null_handle,

texture_image: vk.Image = .null_handle,
texture_image_memory: vk.DeviceMemory = .null_handle,
texture_image_view: vk.ImageView = .null_handle,
texture_image_sampler: vk.Sampler = .null_handle,

depth_image: vk.Image = .null_handle,
depth_image_memory: vk.DeviceMemory = .null_handle,
depth_image_view: vk.ImageView = .null_handle,

vkb: BaseWrapper = undefined,
vki: InstanceWrapper = undefined,
vkd: DeviceWrapper = undefined,
dev: Device = undefined,

dx: f32 = 0.0,
dy: f32 = 0.0,
dz: f32 = 0.0,

pitch: f32 = 0.0,
yaw: f32 = 0.0,

captured_moust: bool = false,

camera_pos: zlm.Vec3 = .new(0.0, 50.0, 3.0),
prev_camera_pos: zlm.Vec3 = .zero,

camera_front: zlm.Vec3 = .new(0.0, 0.0, -1.0),
prev_camera_front: zlm.Vec3 = .new(0.0, 0.0, -1.0),

camera_up: zlm.Vec3 = .unitY,
prev_camera_up: zlm.Vec3 = .unitY,

sensitivity: f32 = 0.1,

mouseX: f32 = 0.0,
mouseY: f32 = 0.0,

image_ready_for_write: [MAX_FRAMES_IN_FLIGHT]vk.Semaphore = @splat(.null_handle),
image_ready_for_present: [MAX_FRAMES_IN_FLIGHT]vk.Semaphore = @splat(.null_handle),
in_flight_fences: [MAX_FRAMES_IN_FLIGHT]vk.Fence = @splat(.null_handle),

frame_buffer_resized: bool = false,

current_frame: usize = 0,

last_frame_time: std.time.Instant = undefined,
time_diff_ns: u64 = 0,

solid_meshes: MeshMap,
transparent_meshes: MeshMap,
meshes_to_regen: util.AutoArrayHashSet(IVec3),
gen_mesh_pool: std.Thread.Pool = undefined,

pub const MeshMap = std.AutoHashMap(IVec3, chunks.Buffers);

const Self = @This();

const Vertex = main.Vertex;

pub const UniformBufferObject = extern struct {
    mvp: zlm.Mat4 align(16),
};

const WIDTH = 800;
const HEIGHT = 600;
pub const MAX_FRAMES_IN_FLIGHT = 2;

const validation_layers: []const [*:0]const u8 = ([_][*:0]const u8{"VK_LAYER_KHRONOS_validation"})[0..];
const device_extensions = [_][:0]const u8{vk.extensions.khr_swapchain.name};
const _device_extension_names_arr = blk: {
    var out: [device_extensions.len][*:0]const u8 = undefined;

    for (device_extensions, 0..) |extension, i| {
        out[i] = extension.ptr;
    }

    break :blk out;
};
const device_extension_names: []const [*:0]const u8 = _device_extension_names_arr[0..];

const enable_validation_layers = switch (builtin.mode) {
    .Debug => true,
    .ReleaseFast => false,
    .ReleaseSafe => true,
    .ReleaseSmall => false,
};

pub fn run(self: *Self) !void {
    self.last_frame_time = try std.time.Instant.now();
    try self.initGame();
    try self.initWindow();
    try self.initVulkan();
    try self.mainLoop();
    self.cleanup();
}

fn initWindow(self: *Self) !void {
    c.glfwInitHint(c.GLFW_PLATFORM, c.GLFW_PLATFORM_X11);

    try glfw.init();
    glfw.windowHint(glfw.ClientAPI, glfw.NoAPI);
    glfw.windowHint(glfw.ClientAPI, glfw.NoAPI);

    self.window = try glfw.createWindow(WIDTH, HEIGHT, "Vulkan", null, null);
    glfw.setWindowUserPointer(self.window, self);

    if (glfw.rawMouseMotionSupported()) {
        std.log.info("Enabled raw input\n", .{});
        glfw.setInputMode(self.window, c.GLFW_RAW_MOUSE_MOTION, c.GLFW_TRUE);
    }

    _ = glfw.setFramebufferSizeCallback(self.window, &framebufferResizeCallback);

    _ = glfw.setKeyCallback(self.window, &keyCallback);
}

fn framebufferResizeCallback(
    window: *glfw.Window,
    width: c_int,
    height: c_int,
) callconv(.c) void {
    _ = width; // autofix
    _ = height; // autofix
    const self: *Self = @ptrCast(@alignCast(glfw.getWindowUserPointer(window).?));

    self.frame_buffer_resized = true;
}

fn getInstanceProcAddress(instance: vk.Instance, procname: [*:0]const u8) callconv(.c) ?*const fn () callconv(.c) void {
    return glfw.getInstanceProcAddress(@intFromEnum(instance), procname);
}

fn initVulkan(self: *Self) !void {
    self.vkb = vk.BaseWrapper.load(getInstanceProcAddress);

    try self.createInstance();
    try self.setupDebugMessenger();
    try self.createSurface();
    try self.pickPhysicalDevice();
    try self.createLogicalDevice();
    try self.createSwapChain();
    try self.createImageViews();
    try self.createRenderPass();
    try self.createDescriptorSetLayout();
    try self.createGraphicsPipeline();
    try self.createCommandPool();
    try self.createDepthResources();
    try self.createFramebuffers();
    try self.createTextureImage();
    try self.createTextureImageView();
    try self.createTextureSampler();
    try self.createVertexBuffer();
    try self.createIndexBuffer();
    try self.createUniformBuffers();
    try self.createDescriptorPool();
    try self.createDescriptorSets();
    try self.createCommandBuffer();
    try self.createSyncObjects();
}

fn createInstance(self: *Self) !void {
    if (enable_validation_layers and !try self.checkValidationLayerSupport()) {
        return error.NoValidationLayers;
    }

    const app_info: vk.ApplicationInfo = .{
        .p_application_name = "Hello Triangle",
        .application_version = @bitCast(vk.makeApiVersion(1, 0, 0, 0)),
        .p_engine_name = "No Engine",
        .engine_version = @bitCast(vk.makeApiVersion(1, 0, 0, 0)),
        .api_version = @bitCast(vk.API_VERSION_1_0),
    };

    var create_info: vk.InstanceCreateInfo = .{
        .p_application_info = &app_info,
    };

    create_info.flags.enumerate_portability_bit_khr = true;

    var required_extensions = try self.getRequiredExtensions();
    defer required_extensions.deinit(self.allocator);
    create_info.enabled_extension_count = @intCast(required_extensions.items.len);
    create_info.pp_enabled_extension_names = required_extensions.items.ptr;

    var debug_create_info: vk.DebugUtilsMessengerCreateInfoEXT = undefined;
    if (enable_validation_layers) {
        create_info.enabled_layer_count = @intCast(validation_layers.len);
        create_info.pp_enabled_layer_names = validation_layers.ptr;

        debug_create_info = populateDebugMessengerCreateInfo();
        create_info.p_next = &debug_create_info;
    } else {
        create_info.enabled_layer_count = 0;

        create_info.p_next = null;
    }

    self.instance = try self.vkb.createInstance(&create_info, null);

    self.vki = .load(self.instance, getInstanceProcAddress);
}

fn checkValidationLayerSupport(self: *Self) !bool {
    var layer_count: u32 = 0;
    _ = try self.vkb.enumerateInstanceLayerProperties(&layer_count, null);

    const available_layers = try self.allocator.alloc(vk.LayerProperties, layer_count);
    defer self.allocator.free(available_layers);
    _ = try self.vkb.enumerateInstanceLayerProperties(&layer_count, available_layers.ptr);

    for (validation_layers) |layer_name| {
        const layer_len = std.mem.len(layer_name);
        var padded_name = try std.ArrayList(u8).initCapacity(self.allocator, 256);
        defer padded_name.deinit(self.allocator);
        padded_name.appendSliceAssumeCapacity(layer_name[0..layer_len]);
        padded_name.appendNTimesAssumeCapacity(0, 256 - layer_len);

        var layer_found = false;
        for (available_layers) |layer_props| {
            if (std.mem.eql(u8, padded_name.items, &layer_props.layer_name)) {
                layer_found = true;
                break;
            }
        }

        if (!layer_found) {
            return false;
        }
    }

    return true;
}

fn getRequiredExtensions(self: *Self) !std.ArrayList([*:0]const u8) {
    var glfw_extension_count: u32 = 0;
    const glfw_extensions_ptr = glfw.getRequiredInstanceExtensions(&glfw_extension_count) orelse {
        return error.NoGlfwExtensions;
    };
    const glfw_extensions = glfw_extensions_ptr[0..glfw_extension_count];

    var extensions = try std.ArrayList([*:0]const u8).initCapacity(
        self.allocator,
        glfw_extension_count + 1,
    );
    errdefer extensions.deinit(self.allocator);

    extensions.appendSliceAssumeCapacity(glfw_extensions);

    if (enable_validation_layers) {
        extensions.appendAssumeCapacity(vk.extensions.ext_debug_utils.name.ptr);
        try extensions.append(self.allocator, vk.extensions.ext_debug_report.name.ptr);
    }

    return extensions;
}

fn debugCallback(
    messageSeverity: vk.DebugUtilsMessageSeverityFlagsEXT,
    message_type: vk.DebugUtilsMessageTypeFlagsEXT,
    p_callback_data: ?*const vk.DebugUtilsMessengerCallbackDataEXT,
    p_user_data: ?*anyopaque,
) callconv(.c) vk.Bool32 {
    _ = message_type; // autofix
    _ = p_user_data; // autofix
    if (p_callback_data) |data| {
        if (data.p_message) |msg| {
            if (messageSeverity.info_bit_ext) {
                std.log.info("Validation layer: {s}", .{msg});
            } else if (messageSeverity.warning_bit_ext) {
                std.log.warn("Validation layer: {s}", .{msg});
            } else if (messageSeverity.error_bit_ext) {
                std.log.err("Validation layer: {s}", .{msg});
            } else if (messageSeverity.verbose_bit_ext) {
                std.log.debug("Validation layer: {s}", .{msg});
            } else {
                std.log.err("Failed to get severity: {any}", .{messageSeverity});
                std.log.err("Validation layer: {s}", .{msg});
            }
        } else {
            std.log.warn("Validation layer: NO MESSAGE", .{});
        }
    }

    return .false;
}

fn setupDebugMessenger(self: *Self) !void {
    if (!enable_validation_layers) {
        return;
    }

    const create_info: vk.DebugUtilsMessengerCreateInfoEXT = populateDebugMessengerCreateInfo();

    std.debug.assert(self.vki.dispatch.vkCreateDebugUtilsMessengerEXT != null);
    self.debugMessenger = try self.vki.createDebugUtilsMessengerEXT(self.instance, &create_info, null);
}

fn populateDebugMessengerCreateInfo() vk.DebugUtilsMessengerCreateInfoEXT {
    return vk.DebugUtilsMessengerCreateInfoEXT{
        .message_severity = .{
            .verbose_bit_ext = true,
            .warning_bit_ext = true,
            .error_bit_ext = true,
        },
        .message_type = .{
            .general_bit_ext = true,
            .validation_bit_ext = true,
            .performance_bit_ext = true,
        },
        .pfn_user_callback = &debugCallback,
        .p_user_data = null,
    };
}

fn pickPhysicalDevice(self: *Self) !void {
    var device_count: u32 = 0;

    _ = try self.vki.enumeratePhysicalDevices(self.instance, &device_count, null);

    if (device_count == 0) {
        std.log.err("Failed to find GPUs with Vulkan support!", .{});
        return error.NoGPU;
    }

    const devices = try self.allocator.alloc(vk.PhysicalDevice, device_count);
    defer self.allocator.free(devices);

    _ = try self.vki.enumeratePhysicalDevices(self.instance, &device_count, devices.ptr);

    var candidates = std.AutoArrayHashMap(i32, vk.PhysicalDevice).init(self.allocator);
    defer candidates.deinit();

    for (devices) |device| {
        const score = self.rateDeviceSuitablility(device);
        try candidates.put(score, device);
    }

    candidates.sort(DeviceSorter{ .hashmap = candidates });

    const first = candidates.keys()[0];
    const first_device = candidates.get(first).?;
    if (try self.isDeviceSuitable(first_device)) {
        self.physical_device = first_device;
    } else {
        std.log.err("Failed to find suitable GPU!", .{});
        return error.NoSuitableGPU;
    }
}

const QueueFamilies = struct {
    graphics_family: ?u32 = null,
    present_family: ?u32 = null,

    fn is_complete(self: *const @This()) bool {
        var out = true;
        inline for (std.meta.fields(@This())) |field| {
            out = out and @field(self, field.name) != null;
        }
        return out;
    }
};

fn findQueueFamilies(self: *const Self, device: vk.PhysicalDevice) !QueueFamilies {
    var queue_indices = QueueFamilies{};

    var queue_family_count: u32 = 0;
    self.vki.getPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, null);

    const queue_families = try self.allocator.alloc(vk.QueueFamilyProperties, queue_family_count);
    defer self.allocator.free(queue_families);
    self.vki.getPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, queue_families.ptr);

    var i: u32 = 0;

    for (queue_families) |queue_family| {
        if (queue_family.queue_flags.graphics_bit) {
            queue_indices.graphics_family = i;
        }

        if (try self.vki.getPhysicalDeviceSurfaceSupportKHR(device, i, self.surface) == .true) {
            queue_indices.present_family = i;
        }

        if (queue_indices.is_complete()) {
            break;
        }

        i += 1;
    }

    return queue_indices;
}

fn isDeviceSuitable(self: *const Self, device: vk.PhysicalDevice) !bool {
    var queue_indices = try self.findQueueFamilies(device);

    const extensionsSupported = try self.checkDeviceExtensionSupport(device);

    var swapChainAdaquate = false;
    if (extensionsSupported) {
        var swapChainSupport = try self.querySwapChainSupport(device);
        defer swapChainSupport.deinit(self.allocator);
        swapChainAdaquate = (swapChainSupport.formats.items.len != 0) and //
            (swapChainSupport.present_modes.items.len != 0);
    }

    const supported_features = self.vki.getPhysicalDeviceFeatures(device);

    return swapChainAdaquate //
    and extensionsSupported //
    and queue_indices.is_complete() //
    and supported_features.sampler_anisotropy == .true;
}

fn checkDeviceExtensionSupport(self: *const Self, device: vk.PhysicalDevice) !bool {
    var extension_count: u32 = 0;

    _ = try self.vki.enumerateDeviceExtensionProperties(device, null, &extension_count, null);

    const available_extensions = try self.allocator.alloc(vk.ExtensionProperties, extension_count);
    defer self.allocator.free(available_extensions);

    _ = try self.vki.enumerateDeviceExtensionProperties(device, null, &extension_count, available_extensions.ptr);

    var required_extensions = std.StringArrayHashMap(void).init(self.allocator);
    defer required_extensions.deinit();

    for (device_extensions) |extension| {
        const extension_len = std.mem.len(extension.ptr);
        try required_extensions.put(extension[0..extension_len], undefined);
    }

    for (available_extensions) |extension| {
        for (required_extensions.keys()) |key| {
            if (std.mem.eql(u8, key, extension.extension_name[0..key.len])) {
                _ = required_extensions.orderedRemove(key);
                break;
            }
        }
    }

    return required_extensions.keys().len == 0;
}

const DeviceSorter = struct {
    hashmap: std.AutoArrayHashMap(i32, vk.PhysicalDevice),

    pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
        const keys = ctx.hashmap.keys();
        const a_key = keys[a_index];
        const b_key = keys[b_index];

        std.log.info("{d} < {d}", .{ a_key, b_key });

        return a_key < b_key;
    }
};

fn rateDeviceSuitablility(self: *const Self, device: vk.PhysicalDevice) i32 {
    const device_properties = self.vki.getPhysicalDeviceProperties(device);

    const device_features = self.vki.getPhysicalDeviceFeatures(device);

    var score: i32 = 0;

    if (device_properties.device_type == .discrete_gpu) {
        score += 1000;
    }

    score += @intCast(device_properties.limits.max_image_dimension_2d);

    if (device_features.geometry_shader == .false) {
        return 0;
    }

    return score;
}

fn createLogicalDevice(self: *Self) !void {
    const queue_indices = try self.findQueueFamilies(self.physical_device);

    var queue_create_infos = std.AutoArrayHashMap(u32, vk.DeviceQueueCreateInfo).init(self.allocator);
    defer queue_create_infos.deinit();

    const queue_priority: f32 = 1.0;
    inline for (std.meta.fields(QueueFamilies)) |field| {
        const queue_family: u32 = @field(queue_indices, field.name).?;
        if (!queue_create_infos.contains(queue_family)) {
            const queue_create_info: vk.DeviceQueueCreateInfo = .{
                .queue_family_index = queue_family,
                .queue_count = 1,
                .p_queue_priorities = @ptrCast(&queue_priority),
            };
            try queue_create_infos.put(queue_family, queue_create_info);
        }
    }

    const device_features: vk.PhysicalDeviceFeatures = .{
        .sampler_anisotropy = .true,
    };

    var create_info: vk.DeviceCreateInfo = .{
        .p_queue_create_infos = queue_create_infos.values().ptr,
        .queue_create_info_count = @intCast(queue_create_infos.values().len),
        .p_enabled_features = &device_features,
        .enabled_extension_count = device_extension_names.len,
        .pp_enabled_extension_names = device_extension_names.ptr,
    };

    if (enable_validation_layers) {
        create_info.enabled_layer_count = @intCast(validation_layers.len);
        create_info.pp_enabled_layer_names = validation_layers.ptr;
    } else {
        create_info.enabled_layer_count = 0;
    }

    self.device = try self.vki.createDevice(self.physical_device, &create_info, null);

    self.vkd = DeviceWrapper.load(self.device, self.vki.dispatch.vkGetDeviceProcAddr.?);
    self.dev = Device.init(self.device, &self.vkd);

    self.graphics_queue = self.vkd.getDeviceQueue(self.device, queue_indices.graphics_family.?, 0);
    self.present_queue = self.vkd.getDeviceQueue(self.device, queue_indices.present_family.?, 0);
}

fn createSurface(self: *Self) !void {
    if (glfw.createWindowSurface(
        @intFromEnum(self.instance),
        self.window.?,
        null,
        @ptrCast(&self.surface),
    ) != .success) {
        std.log.err("Failed to create window surface", .{});
        return error.NoWindowSurface;
    }
}

const SwapChainSuppoertDetails = struct {
    capabilites: vk.SurfaceCapabilitiesKHR,
    formats: std.ArrayList(vk.SurfaceFormatKHR),
    present_modes: std.ArrayList(vk.PresentModeKHR),

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.formats.deinit(allocator);
        self.present_modes.deinit(allocator);
    }
};

fn querySwapChainSupport(
    self: *const Self,
    device: vk.PhysicalDevice,
) !SwapChainSuppoertDetails {
    var details: SwapChainSuppoertDetails = .{
        .capabilites = undefined,
        .formats = .empty,
        .present_modes = .empty,
    };
    errdefer details.formats.deinit(self.allocator);
    errdefer details.present_modes.deinit(self.allocator);

    details.capabilites = try self.vki.getPhysicalDeviceSurfaceCapabilitiesKHR(device, self.surface);

    var format_count: u32 = 0;
    _ = try self.vki.getPhysicalDeviceSurfaceFormatsKHR(device, self.surface, &format_count, null);

    if (format_count != 0) {
        try details.formats.resize(self.allocator, format_count);
        _ = try self.vki.getPhysicalDeviceSurfaceFormatsKHR(
            device,
            self.surface,
            &format_count,
            details.formats.items.ptr,
        );
    }

    var present_mode_count: u32 = 0;
    _ = try self.vki.getPhysicalDeviceSurfacePresentModesKHR(device, self.surface, &present_mode_count, null);

    if (format_count != 0) {
        try details.present_modes.resize(self.allocator, present_mode_count);
        _ = try self.vki.getPhysicalDeviceSurfacePresentModesKHR(
            device,
            self.surface,
            &present_mode_count,
            details.present_modes.items.ptr,
        );
    }

    return details;
}

fn chooseSwapSurfaceFormat(availableFormats: []const vk.SurfaceFormatKHR) vk.SurfaceFormatKHR {
    for (availableFormats) |available_format| {
        if (available_format.format == .b8g8r8a8_srgb and available_format.color_space == .srgb_nonlinear_khr) {
            return available_format;
        }
    }

    return availableFormats[0];
}

fn chooseSwapPresentMode(
    available_present_modes: []const vk.PresentModeKHR,
) vk.PresentModeKHR {
    for (available_present_modes) |present_mode| {
        if (present_mode == .mailbox_khr) {
            return present_mode;
        }
    }

    return .fifo_khr;
}

fn chooseSwapExtent(self: *const Self, capabilities: *const vk.SurfaceCapabilitiesKHR) vk.Extent2D {
    if (capabilities.current_extent.width != std.math.maxInt(u32)) {
        return capabilities.current_extent;
    } else {
        var width: c_int = 0;
        var height: c_int = 0;

        glfw.getFramebufferSize(self.window, &width, &height);

        var actual_extent: vk.Extent2D = .{
            .width = @intCast(width),
            .height = @intCast(height),
        };

        actual_extent.width = std.math.clamp(
            actual_extent.width,
            capabilities.min_image_extent.width,
            capabilities.max_image_extent.width,
        );

        actual_extent.height = std.math.clamp(
            actual_extent.height,
            capabilities.min_image_extent.height,
            capabilities.max_image_extent.height,
        );

        return actual_extent;
    }
}

fn createSwapChain(self: *Self) !void {
    var swap_chain_support = try self.querySwapChainSupport(self.physical_device);
    defer swap_chain_support.deinit(self.allocator);

    const surface_format = chooseSwapSurfaceFormat(swap_chain_support.formats.items);
    const present_mode = chooseSwapPresentMode(swap_chain_support.present_modes.items);
    const extent = self.chooseSwapExtent(&swap_chain_support.capabilites);

    var image_count = swap_chain_support.capabilites.min_image_count + 1;
    if (swap_chain_support.capabilites.max_image_count > 0 and //
        image_count > swap_chain_support.capabilites.max_image_count)
    {
        image_count = swap_chain_support.capabilites.max_image_count;
    }

    var create_info: vk.SwapchainCreateInfoKHR = .{
        .surface = self.surface,
        .min_image_count = image_count,
        .image_format = surface_format.format,
        .image_color_space = surface_format.color_space,
        .image_extent = extent,
        .image_array_layers = 1,
        .image_usage = .{
            .color_attachment_bit = true,
        },
        .pre_transform = swap_chain_support.capabilites.current_transform,
        .composite_alpha = .{
            .opaque_bit_khr = true,
        },
        .present_mode = present_mode,
        .clipped = .true,
        .image_sharing_mode = undefined,
        .old_swapchain = .null_handle,
    };

    const queue_indices = try self.findQueueFamilies(self.physical_device);
    var queue_family_indices = [_]u32{ queue_indices.graphics_family.?, queue_indices.present_family.? };

    if (queue_indices.graphics_family != queue_indices.present_family) {
        create_info.image_sharing_mode = .concurrent;
        create_info.queue_family_index_count = 2;
        create_info.p_queue_family_indices = queue_family_indices[0..].ptr;
    } else {
        create_info.image_sharing_mode = .exclusive;
        create_info.queue_family_index_count = 0;
        create_info.p_queue_family_indices = null;
    }

    self.swapchain = try self.dev.createSwapchainKHR(&create_info, null);

    self.swapchain_images.deinit(self.allocator);
    self.swapchain_images = .fromOwnedSlice(try self.dev.getSwapchainImagesAllocKHR(self.swapchain, self.allocator));
    errdefer self.swapchain_images.deinit(self.allocator);

    self.swapchain_image_format = surface_format.format;
    self.swapchain_extent = extent;
}

fn createImageViews(self: *Self) !void {
    try self.swapchain_image_views.resize(self.allocator, self.swapchain_images.items.len);
    errdefer self.swapchain_image_views.deinit(self.allocator);

    for (self.swapchain_images.items, 0..) |image, i| {
        self.swapchain_image_views.items[i] = try self.createImageView(
            image,
            self.swapchain_image_format,
            .{ .color_bit = true },
        );
    }
}

fn createVertexBuffer(self: *Self) !void {
    _ = self;
    //const r_chunk = state.solidMeshMap.getPtr(.zero).?;

    //try r_chunk.inner.hookupBuffers(
    //    self.vki,
    //    self.dev,
    //    self.physical_device,
    //    self.command_pool,
    //    self.graphics_queue,
    //    self.chunk_descriptor_set_layout,
    //    self.descriptor_pool,
    //);

    //try createVertexBufferGeneric(
    //    self.vki,
    //    self.dev,
    //    self.physical_device,
    //    self.command_pool,
    //    self.graphics_queue,
    //    &self.vertex_buffer,
    //    &self.vertex_buffer_memory,
    //    r_chunk.inner.vertices.items,
    //);
}

pub fn createVertexBufferGeneric(
    vki: InstanceWrapper,
    dev: Device,
    physical_device: vk.PhysicalDevice,
    command_pool: vk.CommandPool,
    queue: vk.Queue,
    buffer: *vk.Buffer,
    memory: *vk.DeviceMemory,
    vertices_in: []const Vertex,
) !void {
    const buffer_size: vk.DeviceSize = @sizeOf(Vertex) * vertices_in.len;

    var staging_buffer: vk.Buffer = .null_handle;
    var staging_buffer_memory: vk.DeviceMemory = .null_handle;

    try createBufferGeneric(
        vki,
        dev,
        physical_device,
        buffer_size,
        .{ .transfer_src_bit = true },
        .{ .host_visible_bit = true, .host_coherent_bit = true },
        &staging_buffer,
        &staging_buffer_memory,
    );

    const data = try dev.mapMemory(staging_buffer_memory, 0, buffer_size, .{});
    const data_arr: [*]Vertex = @ptrCast(@alignCast(data.?));
    @memcpy(data_arr[0..vertices_in.len], vertices_in[0..]);
    dev.unmapMemory(staging_buffer_memory);

    try createBufferGeneric(
        vki,
        dev,
        physical_device,
        buffer_size,
        .{
            .vertex_buffer_bit = true,
            .transfer_dst_bit = true,
        },
        .{ .device_local_bit = true },
        buffer,
        memory,
    );

    try copyBufferGeneric(dev, command_pool, queue, staging_buffer, buffer.*, buffer_size);

    dev.destroyBuffer(staging_buffer, null);
    dev.freeMemory(staging_buffer_memory, null);
}

fn transitionImageLayout(
    self: *Self,
    image: vk.Image,
    format: vk.Format,
    old_layout: vk.ImageLayout,
    new_layout: vk.ImageLayout,
) !void {
    const command_buffer = try self.beginSingleTimeCommands();

    var barrier: vk.ImageMemoryBarrier = .{
        .old_layout = old_layout,
        .new_layout = new_layout,
        .src_queue_family_index = vk.QUEUE_FAMILY_IGNORED,
        .dst_queue_family_index = vk.QUEUE_FAMILY_IGNORED,
        .image = image,
        .subresource_range = .{
            .aspect_mask = .{ .color_bit = true },
            .base_mip_level = 0,
            .level_count = 1,
            .base_array_layer = 0,
            .layer_count = 1,
        },
        .src_access_mask = undefined,
        .dst_access_mask = undefined,
    };

    if (new_layout == .depth_stencil_attachment_optimal) {
        barrier.subresource_range.aspect_mask = .{ .depth_bit = true };

        if (hasStencilComponent(format)) {
            barrier.subresource_range.aspect_mask.stencil_bit = true;
        }
    } else {
        barrier.subresource_range.aspect_mask = .{ .color_bit = true };
    }

    var source_stage: vk.PipelineStageFlags = .{};
    var destination_stage: vk.PipelineStageFlags = .{};

    if (old_layout == .undefined and new_layout == .transfer_dst_optimal) {
        barrier.src_access_mask = .{};
        barrier.dst_access_mask = .{ .transfer_write_bit = true };

        source_stage = .{ .top_of_pipe_bit = true };
        destination_stage = .{ .transfer_bit = true };
    } else if (old_layout == .transfer_dst_optimal and new_layout == .shader_read_only_optimal) {
        barrier.src_access_mask = .{ .transfer_write_bit = true };
        barrier.dst_access_mask = .{ .shader_read_bit = true };

        source_stage = .{ .transfer_bit = true };
        destination_stage = .{ .fragment_shader_bit = true };
    } else if (old_layout == .undefined and new_layout == .depth_stencil_attachment_optimal) {
        barrier.src_access_mask = .{};
        barrier.dst_access_mask = .{
            .depth_stencil_attachment_read_bit = true,
            .depth_stencil_attachment_write_bit = true,
        };

        source_stage = .{ .top_of_pipe_bit = true };
        destination_stage = .{ .early_fragment_tests_bit = true };
    } else {
        std.debug.panic("Failed to transition image layout", .{});
    }

    self.vkd.cmdPipelineBarrier(
        command_buffer,
        source_stage,
        destination_stage,
        .{},
        0,
        null,
        0,
        null,
        1,
        @ptrCast(&barrier),
    );

    try self.endSingleTimeCommands(command_buffer);
}

fn copyBufferToImage(
    self: *Self,
    buffer: vk.Buffer,
    image: vk.Image,
    width: u32,
    height: u32,
) !void {
    const command_buffer = try self.beginSingleTimeCommands();

    const region: vk.BufferImageCopy = .{
        .buffer_offset = 0,
        .buffer_row_length = 0,
        .buffer_image_height = 0,

        .image_subresource = .{
            .aspect_mask = .{ .color_bit = true },
            .mip_level = 0,
            .base_array_layer = 0,
            .layer_count = 1,
        },

        .image_offset = .{ .x = 0, .y = 0, .z = 0 },
        .image_extent = .{ .width = width, .height = height, .depth = 1 },
    };

    self.vkd.cmdCopyBufferToImage(
        command_buffer,
        buffer,
        image,
        .transfer_dst_optimal,
        1,
        @ptrCast(&region),
    );

    try self.endSingleTimeCommands(command_buffer);
}

fn createIndexBuffer(self: *Self) !void {
    _ = self;
    //const r_chunk = state.solidMeshMap.getPtr(.zero).?;

    //try createIndexBufferGeneric(
    //    self.vki,
    //    self.dev,
    //    self.physical_device,
    //    self.command_pool,
    //    self.graphics_queue,
    //    r_chunk.inner.indices.items,
    //    &self.index_buffer,
    //    &self.index_buffer_memory,
    //);
}

pub fn createIndexBufferGeneric(
    vki: InstanceWrapper,
    dev: Device,
    physical_device: vk.PhysicalDevice,
    command_pool: vk.CommandPool,
    queue: vk.Queue,
    in_indices: []const u32,
    buffer: *vk.Buffer,
    memory: *vk.DeviceMemory,
) !void {
    const buffer_size: vk.DeviceSize = @sizeOf(u32) * in_indices.len;

    var staging_buffer: vk.Buffer = .null_handle;
    var staging_buffer_memory: vk.DeviceMemory = .null_handle;

    try createBufferGeneric(
        vki,
        dev,
        physical_device,
        buffer_size,
        .{ .transfer_src_bit = true },
        .{ .host_visible_bit = true, .host_coherent_bit = true },
        &staging_buffer,
        &staging_buffer_memory,
    );

    const data = try dev.mapMemory(staging_buffer_memory, 0, buffer_size, .{});
    const data_arr: [*]u32 = @ptrCast(@alignCast(data.?));
    @memcpy(data_arr[0..in_indices.len], in_indices[0..]);
    dev.unmapMemory(staging_buffer_memory);

    try createBufferGeneric(
        vki,
        dev,
        physical_device,
        buffer_size,
        .{
            .index_buffer_bit = true,
            .transfer_dst_bit = true,
        },
        .{ .device_local_bit = true },
        buffer,
        memory,
    );

    try copyBufferGeneric(dev, command_pool, queue, staging_buffer, buffer.*, buffer_size);

    dev.destroyBuffer(staging_buffer, null);
    dev.freeMemory(staging_buffer_memory, null);
}

fn createUniformBuffers(self: *Self) !void {
    _ = self;
}

fn createDescriptorPool(self: *Self) !void {
    {
        const pool_sizes = [_]vk.DescriptorPoolSize{
            .{
                .type = .combined_image_sampler,
                .descriptor_count = MAX_FRAMES_IN_FLIGHT,
            },
        };

        const pool_info: vk.DescriptorPoolCreateInfo = .{
            .pool_size_count = pool_sizes.len,
            .p_pool_sizes = pool_sizes[0..].ptr,
            .max_sets = MAX_FRAMES_IN_FLIGHT,
            .flags = .{ .free_descriptor_set_bit = true },
        };

        self.descriptor_pool = try self.dev.createDescriptorPool(&pool_info, null);
    }

    {
        const pool_sizes = [_]vk.DescriptorPoolSize{
            .{
                .type = .uniform_buffer,
                .descriptor_count = MAX_FRAMES_IN_FLIGHT * 4096,
            },
        };

        const pool_info: vk.DescriptorPoolCreateInfo = .{
            .pool_size_count = pool_sizes.len,
            .p_pool_sizes = pool_sizes[0..].ptr,
            .max_sets = MAX_FRAMES_IN_FLIGHT * 4096,
            .flags = .{ .free_descriptor_set_bit = true },
        };

        self.chunks_descriptor_pool = try self.dev.createDescriptorPool(&pool_info, null);
    }
}

fn createDescriptorSets(self: *Self) !void {
    var layouts: [MAX_FRAMES_IN_FLIGHT]vk.DescriptorSetLayout = @splat(self.descriptor_set_layout);
    const alloc_info: vk.DescriptorSetAllocateInfo = .{
        .descriptor_pool = self.descriptor_pool,
        .descriptor_set_count = MAX_FRAMES_IN_FLIGHT,
        .p_set_layouts = layouts[0..].ptr,
    };

    try self.dev.allocateDescriptorSets(&alloc_info, self.descriptor_sets[0..].ptr);

    for (0..MAX_FRAMES_IN_FLIGHT) |i| {
        const image_info: vk.DescriptorImageInfo = .{
            .image_layout = .shader_read_only_optimal,
            .image_view = self.texture_image_view,
            .sampler = self.texture_image_sampler,
        };

        const descriptor_writes = [_]vk.WriteDescriptorSet{
            .{
                .dst_set = self.descriptor_sets[i],
                .dst_binding = 0,
                .dst_array_element = 0,
                .descriptor_type = .combined_image_sampler,
                .descriptor_count = 1,
                .p_buffer_info = ([_]vk.DescriptorBufferInfo{})[0..].ptr,
                .p_image_info = @ptrCast(&image_info),
                .p_texel_buffer_view = ([_]vk.BufferView{})[0..].ptr,
            },
        };

        self.dev.updateDescriptorSets(
            @intCast(descriptor_writes.len),
            descriptor_writes[0..].ptr,
            0,
            null,
        );
    }
}

fn copyBuffer(
    self: *Self,
    src_buffer: vk.Buffer,
    dst_buffer: vk.Buffer,
    size: vk.DeviceSize,
) !void {
    try copyBufferGeneric(
        self.dev,
        self.command_pool,
        self.graphics_queue,
        src_buffer,
        dst_buffer,
        size,
    );
}

fn createBuffer(
    self: *Self,
    size: vk.DeviceSize,
    usage: vk.BufferUsageFlags,
    properties: vk.MemoryPropertyFlags,
    buffer: *vk.Buffer,
    buffer_memory: *vk.DeviceMemory,
) !void {
    return createBufferGeneric(
        self.vki,
        self.dev,
        self.physical_device,
        size,
        usage,
        properties,
        buffer,
        buffer_memory,
    );
}

pub fn createBufferGeneric(
    vki: InstanceWrapper,
    dev: Device,
    physical_device: vk.PhysicalDevice,
    size: vk.DeviceSize,
    usage: vk.BufferUsageFlags,
    properties: vk.MemoryPropertyFlags,
    buffer: *vk.Buffer,
    buffer_memory: *vk.DeviceMemory,
) !void {
    const buffer_info: vk.BufferCreateInfo = .{
        .size = size,
        .usage = usage,
        .sharing_mode = .exclusive,
    };

    buffer.* = try dev.createBuffer(&buffer_info, null);

    const mem_requirements = dev.getBufferMemoryRequirements(buffer.*);

    const alloc_info: vk.MemoryAllocateInfo = .{
        .allocation_size = mem_requirements.size,
        .memory_type_index = try findMemoryTypeGeneric(
            vki,
            physical_device,
            mem_requirements.memory_type_bits,
            properties,
        ),
    };

    buffer_memory.* = try dev.allocateMemory(&alloc_info, null);

    try dev.bindBufferMemory(buffer.*, buffer_memory.*, 0);
}

fn findMemoryType(self: *Self, type_filter: u32, properties: vk.MemoryPropertyFlags) !u32 {
    return findMemoryTypeGeneric(self.vki, self.physical_device, type_filter, properties);
}

fn findMemoryTypeGeneric(
    vki: InstanceWrapper,
    physical_device: vk.PhysicalDevice,
    type_filter: u32,
    properties: vk.MemoryPropertyFlags,
) !u32 {
    const mem_properties = vki.getPhysicalDeviceMemoryProperties(physical_device);

    for (0..mem_properties.memory_type_count) |i| {
        if (type_filter & (@as(u32, 1) << @intCast(i)) != 0 and //
            @as(u32, @bitCast(mem_properties.memory_types[i].property_flags)) //
            & @as(u32, @bitCast(properties)) == @as(u32, @bitCast(properties)))
        {
            return @intCast(i);
        }
    }

    std.log.err("Failed to find suitable memory type", .{});
    return error.NoSuitableMemoryType;
}

fn createDescriptorSetLayout(self: *Self) !void {
    const ubo_layout_binding: vk.DescriptorSetLayoutBinding = .{
        .binding = 0,
        .descriptor_type = .uniform_buffer,
        .descriptor_count = 1,
        .stage_flags = .{ .vertex_bit = true },
        .p_immutable_samplers = null,
    };

    const sampler_layout_binding: vk.DescriptorSetLayoutBinding = .{
        .binding = 0,
        .descriptor_count = 1,
        .descriptor_type = .combined_image_sampler,
        .p_immutable_samplers = null,
        .stage_flags = .{ .fragment_bit = true },
    };

    {
        const bindings = [_]vk.DescriptorSetLayoutBinding{sampler_layout_binding};

        const layout_info: vk.DescriptorSetLayoutCreateInfo = .{
            .binding_count = @intCast(bindings.len),
            .p_bindings = bindings[0..].ptr,
        };

        self.descriptor_set_layout = try self.dev.createDescriptorSetLayout(
            &layout_info,
            null,
        );
    }

    {
        const bindings = [_]vk.DescriptorSetLayoutBinding{ubo_layout_binding};

        const layout_info: vk.DescriptorSetLayoutCreateInfo = .{
            .binding_count = @intCast(bindings.len),
            .p_bindings = bindings[0..].ptr,
        };

        self.chunk_descriptor_set_layout = try self.dev.createDescriptorSetLayout(
            &layout_info,
            null,
        );
    }
}

fn createGraphicsPipeline(self: *Self) !void {
    const vertex_shader align(4) = @embedFile("vertex_shader").*;
    const fragment_shader align(4) = @embedFile("fragment_shader").*;

    const vert_shader_module = try self.createShaderModule(&vertex_shader);
    defer self.dev.destroyShaderModule(vert_shader_module, null);
    const frag_shader_module = try self.createShaderModule(&fragment_shader);
    defer self.dev.destroyShaderModule(frag_shader_module, null);

    const vert_shader_stage_info: vk.PipelineShaderStageCreateInfo = .{
        .stage = .{ .vertex_bit = true },
        .module = vert_shader_module,
        .p_name = "main",
    };

    const frag_shader_stage_info: vk.PipelineShaderStageCreateInfo = .{
        .stage = .{ .fragment_bit = true },
        .module = frag_shader_module,
        .p_name = "main",
    };

    const shader_stages = [_]vk.PipelineShaderStageCreateInfo{
        vert_shader_stage_info,
        frag_shader_stage_info,
    };

    const dynamic_states = [_]vk.DynamicState{
        .viewport,
        .scissor,
    };

    const dynamic_state: vk.PipelineDynamicStateCreateInfo = .{
        .dynamic_state_count = @intCast(dynamic_states.len),
        .p_dynamic_states = dynamic_states[0..].ptr,
    };

    const binding_description = Vertex.getBindingDescription();
    const attribute_description = Vertex.getAttributeDescriptions();

    const vertex_input_info: vk.PipelineVertexInputStateCreateInfo = .{
        .vertex_binding_description_count = 1,
        .p_vertex_binding_descriptions = @ptrCast(&binding_description),
        .vertex_attribute_description_count = attribute_description.len,
        .p_vertex_attribute_descriptions = attribute_description[0..].ptr,
    };

    const input_assembly: vk.PipelineInputAssemblyStateCreateInfo = .{
        .topology = .triangle_list,
        .primitive_restart_enable = .false,
    };

    const viewport: vk.Viewport = .{
        .x = 0,
        .y = 0,
        .width = @floatFromInt(self.swapchain_extent.width),
        .height = @floatFromInt(self.swapchain_extent.height),
        .min_depth = 0.0,
        .max_depth = 1.0,
    };

    const scisor: vk.Rect2D = .{
        .offset = .{ .x = 0, .y = 0 },
        .extent = self.swapchain_extent,
    };

    const viewport_state: vk.PipelineViewportStateCreateInfo = .{
        .viewport_count = 1,
        .scissor_count = 1,
        .p_viewports = @ptrCast(&viewport),
        .p_scissors = @ptrCast(&scisor),
    };

    const rasterizer: vk.PipelineRasterizationStateCreateInfo = .{
        .depth_bias_enable = .false,
        .depth_clamp_enable = .false,
        .depth_bias_constant_factor = 0.0,
        .depth_bias_clamp = 0.0,
        .depth_bias_slope_factor = 0.0,
        .rasterizer_discard_enable = .false,
        .polygon_mode = .fill,
        .line_width = 1.0,
        .cull_mode = .{ .back_bit = true },
        .front_face = .clockwise,
    };

    const multisampling: vk.PipelineMultisampleStateCreateInfo = .{
        .sample_shading_enable = .true,
        .rasterization_samples = .{ .@"1_bit" = true },
        .min_sample_shading = 1.0,
        .alpha_to_coverage_enable = .false,
        .alpha_to_one_enable = .false,
    };

    const color_blend_attachment: vk.PipelineColorBlendAttachmentState = .{
        .color_write_mask = .{
            .r_bit = true,
            .g_bit = true,
            .b_bit = true,
            .a_bit = true,
        },
        .blend_enable = .true,
        .src_color_blend_factor = .src_alpha,
        .dst_color_blend_factor = .one_minus_src_alpha,
        .color_blend_op = .add,
        .src_alpha_blend_factor = .one,
        .dst_alpha_blend_factor = .zero,
        .alpha_blend_op = .add,
    };

    const color_blending: vk.PipelineColorBlendStateCreateInfo = .{
        .logic_op_enable = .false,
        .logic_op = .copy,
        .attachment_count = 1,
        .p_attachments = @ptrCast(&color_blend_attachment),
        .blend_constants = @splat(0.0),
    };

    const descriptor_set_layouts = [_]vk.DescriptorSetLayout{
        self.chunk_descriptor_set_layout,
        self.descriptor_set_layout,
    };

    const pipeline_layout_info: vk.PipelineLayoutCreateInfo = .{
        .set_layout_count = descriptor_set_layouts.len,
        .p_set_layouts = descriptor_set_layouts[0..].ptr,
    };

    self.pipeline_layout = try self.dev.createPipelineLayout(
        &pipeline_layout_info,
        null,
    );

    const depth_stencil: vk.PipelineDepthStencilStateCreateInfo = .{
        .depth_test_enable = .true,
        .depth_write_enable = .true,
        .depth_compare_op = .less,
        .depth_bounds_test_enable = .false,
        .min_depth_bounds = 0.0,
        .max_depth_bounds = 1.0,
        .stencil_test_enable = .true,
        .front = undefined,
        .back = undefined,
    };

    const pipeline_info: vk.GraphicsPipelineCreateInfo = .{
        .stage_count = 2,
        .p_stages = shader_stages[0..].ptr,
        .p_vertex_input_state = &vertex_input_info,
        .p_input_assembly_state = &input_assembly,
        .p_rasterization_state = &rasterizer,
        .p_multisample_state = &multisampling,
        .p_depth_stencil_state = &depth_stencil,
        .p_color_blend_state = &color_blending,
        .p_dynamic_state = &dynamic_state,
        .p_viewport_state = &viewport_state,
        .layout = self.pipeline_layout,
        .render_pass = self.render_pass,
        .subpass = 0,
        .base_pipeline_handle = .null_handle,
        .base_pipeline_index = -1,
    };

    _ = try self.dev.createGraphicsPipelines(
        .null_handle,
        1,
        @ptrCast(&pipeline_info),
        null,
        @ptrCast(&self.graphics_pipeline),
    );
}

fn createRenderPass(self: *Self) !void {
    const color_attachment: vk.AttachmentDescription = .{
        .format = self.swapchain_image_format,
        .samples = .{ .@"1_bit" = true },
        .load_op = .clear,
        .store_op = .store,
        .stencil_load_op = .dont_care,
        .stencil_store_op = .dont_care,
        .initial_layout = .undefined,
        .final_layout = .present_src_khr,
    };

    const color_attachment_ref: vk.AttachmentReference = .{
        .attachment = 0,
        .layout = .color_attachment_optimal,
    };

    const depth_attachment: vk.AttachmentDescription = .{
        .format = self.findDepthFormat(),
        .samples = .{ .@"1_bit" = true },
        .load_op = .clear,
        .store_op = .dont_care,
        .stencil_load_op = .dont_care,
        .stencil_store_op = .dont_care,
        .initial_layout = .undefined,
        .final_layout = .depth_stencil_attachment_optimal,
    };

    const depth_attachment_ref: vk.AttachmentReference = .{
        .attachment = 1,
        .layout = .depth_stencil_attachment_optimal,
    };

    const subpass: vk.SubpassDescription = .{
        .pipeline_bind_point = .graphics,
        .color_attachment_count = 1,
        .p_color_attachments = @ptrCast(&color_attachment_ref),
        .p_depth_stencil_attachment = &depth_attachment_ref,
    };

    const dependency: vk.SubpassDependency = .{
        .src_subpass = vk.SUBPASS_EXTERNAL,
        .dst_subpass = 0,
        .src_stage_mask = .{
            .color_attachment_output_bit = true,
            .late_fragment_tests_bit = true,
        },
        .src_access_mask = .{
            .depth_stencil_attachment_write_bit = true,
        },
        .dst_stage_mask = .{
            .color_attachment_output_bit = true,
            .early_fragment_tests_bit = true,
        },
        .dst_access_mask = .{
            .color_attachment_write_bit = true,
            .depth_stencil_attachment_write_bit = true,
        },
    };

    const attachments = [_]vk.AttachmentDescription{
        color_attachment,
        depth_attachment,
    };

    const render_pass_info: vk.RenderPassCreateInfo = .{
        .attachment_count = attachments.len,
        .p_attachments = attachments[0..].ptr,
        .subpass_count = 1,
        .p_subpasses = @ptrCast(&subpass),
        .dependency_count = 1,
        .p_dependencies = @ptrCast(&dependency),
    };

    self.render_pass = try self.dev.createRenderPass(&render_pass_info, null);
}

fn createShaderModule(self: *const Self, code: []align(4) const u8) !vk.ShaderModule {
    const create_info: vk.ShaderModuleCreateInfo = .{
        .code_size = code.len,
        .p_code = @ptrCast(code.ptr),
    };

    return self.dev.createShaderModule(&create_info, null);
}

fn createFramebuffers(self: *Self) !void {
    try self.swapchain_framebuffers.resize(
        self.allocator,
        self.swapchain_image_views.items.len,
    );
    errdefer self.swapchain_framebuffers.deinit(self.allocator);

    for (self.swapchain_image_views.items, 0..) |image_view, i| {
        const attachments = [_]vk.ImageView{
            image_view,
            self.depth_image_view,
        };

        const frame_buffer_info: vk.FramebufferCreateInfo = .{
            .render_pass = self.render_pass,
            .attachment_count = attachments.len,
            .p_attachments = attachments[0..].ptr,
            .width = self.swapchain_extent.width,
            .height = self.swapchain_extent.height,
            .layers = 1,
        };

        self.swapchain_framebuffers.items[i] = try self.dev.createFramebuffer(
            &frame_buffer_info,
            null,
        );
    }
}

fn createCommandPool(self: *Self) !void {
    const queue_family_indices = try self.findQueueFamilies(self.physical_device);
    const pool_info: vk.CommandPoolCreateInfo = .{
        .flags = .{ .reset_command_buffer_bit = true },
        .queue_family_index = queue_family_indices.graphics_family.?,
    };

    self.command_pool = try self.dev.createCommandPool(&pool_info, null);
}

fn createDepthResources(self: *Self) !void {
    const depth_format = self.findDepthFormat();

    try self.createImage(
        self.swapchain_extent.width,
        self.swapchain_extent.height,
        depth_format,
        .optimal,
        .{ .depth_stencil_attachment_bit = true },
        .{ .device_local_bit = true },
        &self.depth_image,
        &self.depth_image_memory,
    );

    self.depth_image_view = try self.createImageView(
        self.depth_image,
        depth_format,
        .{ .depth_bit = true },
    );

    try self.transitionImageLayout(
        self.depth_image,
        depth_format,
        .undefined,
        .depth_stencil_attachment_optimal,
    );
}

fn findSupportedFormat(
    self: *Self,
    candidates: []const vk.Format,
    tiling: vk.ImageTiling,
    featrues: vk.FormatFeatureFlags,
) vk.Format {
    for (candidates) |format| {
        const props = self.vki.getPhysicalDeviceFormatProperties(self.physical_device, format);

        if (tiling == .linear and atLeast(props.linear_tiling_features, featrues)) {
            return format;
        } else if (tiling == .optimal and atLeast(props.optimal_tiling_features, featrues)) {
            return format;
        }
    }

    std.debug.panic("Failed to find supported format", .{});
}

fn hasStencilComponent(format: vk.Format) bool {
    return format == .d32_sfloat_s8_uint or format == .d24_unorm_s8_uint;
}

fn findDepthFormat(self: *Self) vk.Format {
    const candidates = [_]vk.Format{
        .d32_sfloat,
        .d32_sfloat_s8_uint,
        .d24_unorm_s8_uint,
    };

    return self.findSupportedFormat(
        candidates[0..],
        .optimal,
        .{ .depth_stencil_attachment_bit = true },
    );
}

fn atLeast(input: anytype, at_least: @TypeOf(input)) bool {
    const T = @TypeOf(input);
    const t_info = @typeInfo(T);
    const struct_info = t_info.@"struct";

    const T_int = struct_info.backing_integer.?;

    return (@as(T_int, @bitCast(input)) & @as(T_int, @bitCast(at_least))) //
    == @as(T_int, @bitCast(at_least));
}

fn createTextureImage(self: *Self) !void {
    const image_size = state.atlas.len;

    const tex_width = 32;
    const tex_height = image_size / 32;

    var staging_buffer: vk.Buffer = .null_handle;
    var staging_buffer_memory: vk.DeviceMemory = .null_handle;

    try self.createBuffer(
        image_size,
        .{ .transfer_src_bit = true },
        .{ .host_visible_bit = true, .host_coherent_bit = true },
        &staging_buffer,
        &staging_buffer_memory,
    );

    try self.copyData(u8, state.atlas, staging_buffer_memory, image_size);

    try self.createImage(
        @intCast(tex_width),
        @intCast(tex_height),
        .r8g8b8a8_srgb,
        .optimal,
        vk.ImageUsageFlags{
            .transfer_dst_bit = true,
            .sampled_bit = true,
        },
        .{ .device_local_bit = true },
        &self.texture_image,
        &self.texture_image_memory,
    );

    try self.transitionImageLayout(
        self.texture_image,
        .r8g8b8a8_srgb,
        .undefined,
        .transfer_dst_optimal,
    );

    try self.copyBufferToImage(
        staging_buffer,
        self.texture_image,
        @intCast(tex_width),
        @intCast(tex_height),
    );

    try self.transitionImageLayout(
        self.texture_image,
        .r8g8b8a8_srgb,
        .transfer_dst_optimal,
        .shader_read_only_optimal,
    );

    self.dev.destroyBuffer(staging_buffer, null);
    self.dev.freeMemory(staging_buffer_memory, null);
}

fn createTextureImageView(self: *Self) !void {
    self.texture_image_view = try self.createImageView(
        self.texture_image,
        .r8g8b8a8_srgb,
        .{ .color_bit = true },
    );
}

fn createTextureSampler(self: *Self) !void {
    const properties = self.vki.getPhysicalDeviceProperties(self.physical_device);

    const sampler_info: vk.SamplerCreateInfo = .{
        .mag_filter = .nearest,
        .min_filter = .nearest,
        .address_mode_u = .repeat,
        .address_mode_v = .repeat,
        .address_mode_w = .repeat,
        .anisotropy_enable = .true,
        .max_anisotropy = properties.limits.max_sampler_anisotropy,
        .border_color = .float_opaque_black,
        .unnormalized_coordinates = .false,
        .compare_enable = .false,
        .compare_op = .always,
        .mipmap_mode = .linear,
        .mip_lod_bias = 0.0,
        .min_lod = 0.0,
        .max_lod = 0.0,
    };

    self.texture_image_sampler = try self.dev.createSampler(&sampler_info, null);
}

fn createImageView(
    self: *Self,
    image: vk.Image,
    format: vk.Format,
    aspect_flags: vk.ImageAspectFlags,
) !vk.ImageView {
    const view_info: vk.ImageViewCreateInfo = .{
        .image = image,
        .view_type = .@"2d",
        .format = format,
        .subresource_range = .{
            .aspect_mask = aspect_flags,
            .base_mip_level = 0,
            .level_count = 1,
            .base_array_layer = 0,
            .layer_count = 1,
        },
        .components = .{
            .r = .identity,
            .g = .identity,
            .b = .identity,
            .a = .identity,
        },
    };

    return self.dev.createImageView(&view_info, null);
}

fn beginSingleTimeCommands(self: *Self) !vk.CommandBuffer {
    return beginSingleTimeCommandsGeneric(self.dev, self.command_pool);
}

fn endSingleTimeCommands(self: *Self, command_buffer: vk.CommandBuffer) !void {
    try endSingleTimeCommandsGeneric(
        self.dev,
        self.command_pool,
        self.graphics_queue,
        command_buffer,
    );
}

fn createImage(
    self: *Self,
    width: u32,
    height: u32,
    format: vk.Format,
    tiling: vk.ImageTiling,
    usage: vk.ImageUsageFlags,
    properties: vk.MemoryPropertyFlags,
    image: *vk.Image,
    image_memory: *vk.DeviceMemory,
) !void {
    const image_info: vk.ImageCreateInfo = .{
        .image_type = .@"2d",
        .extent = .{
            .width = width,
            .height = height,
            .depth = 1,
        },
        .mip_levels = 1,
        .array_layers = 1,
        .format = format,
        .tiling = tiling,
        .initial_layout = .undefined,
        .usage = usage,
        .samples = .{ .@"1_bit" = true },
        .sharing_mode = .exclusive,
    };

    image.* = try self.dev.createImage(&image_info, null);

    const mem_requirements = self.dev.getImageMemoryRequirements(image.*);

    const alloc_info: vk.MemoryAllocateInfo = .{
        .allocation_size = mem_requirements.size,
        .memory_type_index = try self.findMemoryType(
            mem_requirements.memory_type_bits,
            properties,
        ),
    };

    image_memory.* = try self.dev.allocateMemory(&alloc_info, null);

    try self.dev.bindImageMemory(image.*, image_memory.*, 0);
}

fn copyData(
    self: *const Self,
    comptime T: type,
    data: []const T,
    memory: vk.DeviceMemory,
    size: usize,
) !void {
    if (data.len != size) {
        return error.WrongLen;
    }
    const mapped_memory = try self.dev.mapMemory(memory, 0, size, .{});
    const mapped_memory_buffer: [*]T = @ptrCast(@alignCast(mapped_memory));
    @memcpy(mapped_memory_buffer[0..size], data);
    self.dev.unmapMemory(memory);
}

fn createCommandBuffer(self: *Self) !void {
    const alloc_info: vk.CommandBufferAllocateInfo = .{
        .command_pool = self.command_pool,
        .level = .primary,
        .command_buffer_count = self.command_buffers.len,
    };

    _ = try self.dev.allocateCommandBuffers(&alloc_info, self.command_buffers[0..].ptr);
}

fn recordCommandBuffer(
    self: *Self,
    command_buffer: vk.CommandBuffer,
    image_index: u32,
) !void {
    const begin_info: vk.CommandBufferBeginInfo = .{};

    _ = try self.dev.beginCommandBuffer(command_buffer, &begin_info);

    const clear_values = [_]vk.ClearValue{
        .{
            .color = .{ .float_32 = .{ 0.0, 0.0, 0.0, 1.0 } },
        },
        .{
            .depth_stencil = .{ .depth = 1.0, .stencil = 0 },
        },
    };

    const render_pass_info: vk.RenderPassBeginInfo = .{
        .render_pass = self.render_pass,
        .framebuffer = self.swapchain_framebuffers.items[image_index],
        .render_area = .{
            .offset = .{ .x = 0, .y = 0 },
            .extent = self.swapchain_extent,
        },
        .clear_value_count = clear_values.len,
        .p_clear_values = clear_values[0..].ptr,
    };

    self.vkd.cmdBeginRenderPass(command_buffer, &render_pass_info, .@"inline");

    self.vkd.cmdBindPipeline(command_buffer, .graphics, self.graphics_pipeline);

    var chunk_iter = self.solid_meshes.iterator();
    while (chunk_iter.next()) |entry| {
        const chunk_pos = chunks.chunkToWorldPos(entry.key_ptr.*);
        const key = chunk_pos;
        const r_chunk = entry.value_ptr;

        const ubo: UniformBufferObject = .{
            .mvp = self.computeVsParams(key.x, key.y, key.z),
        };
        r_chunk.updateUniformBuffer(self.current_frame, ubo);
        const vertex_buffers = [_]vk.Buffer{r_chunk.vertexBuffer};

        const offsets = [_]vk.DeviceSize{0};

        self.vkd.cmdBindVertexBuffers(
            command_buffer,
            0,
            1,
            vertex_buffers[0..].ptr,
            offsets[0..].ptr,
        );

        self.vkd.cmdBindIndexBuffer(
            command_buffer,
            r_chunk.indexBuffer,
            0,
            .uint32,
        );

        const viewport: vk.Viewport = .{
            .x = 0.0,
            .y = 0.0,
            .width = @floatFromInt(self.swapchain_extent.width),
            .height = @floatFromInt(self.swapchain_extent.height),
            .min_depth = 0.0,
            .max_depth = 1.0,
        };

        self.vkd.cmdSetViewport(command_buffer, 0, 1, @ptrCast(&viewport));

        const scissor: vk.Rect2D = .{
            .offset = .{ .x = 0, .y = 0 },
            .extent = self.swapchain_extent,
        };

        self.vkd.cmdSetScissor(command_buffer, 0, 1, @ptrCast(&scissor));

        const descriptor_sets = [_]vk.DescriptorSet{
            r_chunk.descriptor_sets[self.current_frame],
            self.descriptor_sets[self.current_frame],
        };

        self.vkd.cmdBindDescriptorSets(
            command_buffer,
            .graphics,
            self.pipeline_layout,
            0,
            descriptor_sets.len,
            descriptor_sets[0..].ptr,
            0,
            null,
        );

        self.vkd.cmdDrawIndexed(command_buffer, @intCast(r_chunk.index_count), 1, 0, 0, 0);
    }

    var trans_chunk_iter = self.transparent_meshes.iterator();
    while (trans_chunk_iter.next()) |entry| {
        const chunk_pos = chunks.chunkToWorldPos(entry.key_ptr.*);
        const key = chunk_pos;
        const r_chunk = entry.value_ptr;

        const ubo: UniformBufferObject = .{
            .mvp = self.computeVsParams(key.x, key.y, key.z),
        };
        r_chunk.updateUniformBuffer(self.current_frame, ubo);

        const vertex_buffers = [_]vk.Buffer{r_chunk.vertexBuffer};

        const offsets = [_]vk.DeviceSize{0};

        self.vkd.cmdBindVertexBuffers(
            command_buffer,
            0,
            1,
            vertex_buffers[0..].ptr,
            offsets[0..].ptr,
        );

        self.vkd.cmdBindIndexBuffer(
            command_buffer,
            r_chunk.indexBuffer,
            0,
            .uint32,
        );

        const viewport: vk.Viewport = .{
            .x = 0.0,
            .y = 0.0,
            .width = @floatFromInt(self.swapchain_extent.width),
            .height = @floatFromInt(self.swapchain_extent.height),
            .min_depth = 0.0,
            .max_depth = 1.0,
        };

        self.vkd.cmdSetViewport(command_buffer, 0, 1, @ptrCast(&viewport));

        const scissor: vk.Rect2D = .{
            .offset = .{ .x = 0, .y = 0 },
            .extent = self.swapchain_extent,
        };

        self.vkd.cmdSetScissor(command_buffer, 0, 1, @ptrCast(&scissor));

        const descriptor_sets = [_]vk.DescriptorSet{
            r_chunk.descriptor_sets[self.current_frame],
            self.descriptor_sets[self.current_frame],
        };

        self.vkd.cmdBindDescriptorSets(
            command_buffer,
            .graphics,
            self.pipeline_layout,
            0,
            descriptor_sets.len,
            descriptor_sets[0..].ptr,
            0,
            null,
        );

        self.vkd.cmdDrawIndexed(command_buffer, @intCast(r_chunk.index_count), 1, 0, 0, 0);
    }

    self.vkd.cmdEndRenderPass(command_buffer);

    try self.vkd.endCommandBuffer(command_buffer);
}

fn createSyncObjects(self: *Self) !void {
    const semaphore_info: vk.SemaphoreCreateInfo = .{};
    const fence_info: vk.FenceCreateInfo = .{
        .flags = .{ .signaled_bit = true },
    };

    for (0..MAX_FRAMES_IN_FLIGHT) |i| {
        self.image_ready_for_present[i] = try self.dev.createSemaphore(&semaphore_info, null);
        self.image_ready_for_write[i] = try self.dev.createSemaphore(&semaphore_info, null);
        self.in_flight_fences[i] = try self.dev.createFence(&fence_info, null);
    }
}

fn genMesh(self: *Self, chunk_pos: IVec3) !void {
    std.debug.print("Generating mesh at: {f}\n", .{chunk_pos});
    const neighbors = chunks.NeighborChunks{
        .x = state.chunkMap.getPtr(chunk_pos.add(.unitX)),
        .neg_x = state.chunkMap.getPtr(chunk_pos.sub(.unitX)),
        .z = state.chunkMap.getPtr(chunk_pos.add(.unitZ)),
        .neg_z = state.chunkMap.getPtr(chunk_pos.sub(.unitZ)),
    };
    const sides = try chunks.genMeshSidesGeneric(neighbors);
    const chunk = state.chunkMap.getPtr(chunk_pos).?;
    var solid_buffer = chunks.Chunk.ChunkBuffer{
        .indexBuffer = try .initCapacity(self.allocator, 32000),
        .vertexBuffer = try .initCapacity(self.allocator, 32000),
    };
    defer solid_buffer.deinit(self.allocator);

    var transparent_buffer = chunks.Chunk.ChunkBuffer{
        .indexBuffer = try .initCapacity(self.allocator, 32000),
        .vertexBuffer = try .initCapacity(self.allocator, 32000),
    };
    defer transparent_buffer.deinit(self.allocator);
    try chunk.gen_mesh(sides, self.allocator, &solid_buffer, &transparent_buffer);

    if (self.solid_meshes.fetchRemove(chunk_pos)) |kv| {
        var mesh = kv.value;
        try mesh.deinit(self.dev, self.descriptor_pool);
    }

    if (self.transparent_meshes.fetchRemove(chunk_pos)) |kv| {
        var mesh = kv.value;
        try mesh.deinit(self.dev, self.descriptor_pool);
    }

    try self.solid_meshes.put(chunk_pos, try .init(
        solid_buffer.vertexBuffer.items,
        solid_buffer.indexBuffer.items,
        self.vki,
        self.dev,
        self.physical_device,
        self.command_pool,
        self.graphics_queue,
        self.chunk_descriptor_set_layout,
        self.chunks_descriptor_pool,
    ));

    if (transparent_buffer.indexBuffer.items.len > 0) {
        try self.transparent_meshes.put(chunk_pos, try .init(
            transparent_buffer.vertexBuffer.items,
            transparent_buffer.indexBuffer.items,
            self.vki,
            self.dev,
            self.physical_device,
            self.command_pool,
            self.graphics_queue,
            self.chunk_descriptor_set_layout,
            self.chunks_descriptor_pool,
        ));
    }
}

fn genMeshPanic(self: *Self, pos: IVec3) void {
    self.genMesh(pos) catch unreachable;
}

fn mainLoop(self: *Self) !void {
    try self.gen_mesh_pool.init(.{ .allocator = self.allocator });

    while (!glfw.windowShouldClose(self.window)) {
        self.time_diff_ns = (try std.time.Instant.now()).since(self.last_frame_time);
        self.last_frame_time = try .now();
        glfw.pollEvents();

        try self.playerMovement();
        try self.drawFrame();
        try self.renderDistanceGen();
        try self.getFromOtherThread();
        try self.genMeshes();
    }
    try self.dev.deviceWaitIdle();

    self.gen_mesh_pool.deinit();
}

fn getFromOtherThread(self: *Self) !void {
    while (state.recvWorkerThreadQueue.dequeue()) |msg| {
        switch (msg) {
            .NewChunk => |chunk| {
                _ = state.chunksInFlightSet.remove(chunk.pos);
                try state.chunkMap.put(chunk.pos, chunk.chunk);
                try self.meshes_to_regen.put(chunk.pos, .{});
                try chunks.regenNeighborMeshesGeneric(&self.meshes_to_regen, chunk.pos);
            },
        }
    }
}

fn genMeshes(self: *Self) !void {
    const chunk_poss = self.meshes_to_regen.keys();

    for (chunk_poss) |pos| {
        if (!state.chunkMap.contains(pos)) continue;
        try self.genMesh(pos);
    }

    self.meshes_to_regen.clearRetainingCapacity();
}

fn drawFrame(self: *Self) !void {
    _ = try self.dev.waitForFences(1, self.in_flight_fences[self.current_frame..].ptr, .true, std.math.maxInt(u64));

    const next_image_result = (try self.dev.acquireNextImageKHR(
        self.swapchain,
        std.math.maxInt(u64),
        self.image_ready_for_write[self.current_frame],
        self.in_flight_fences[self.current_frame],
    ));

    if (next_image_result.result == .error_out_of_date_khr) {
        try self.recreateSwapChain();
        return;
    }

    try self.dev.resetFences(1, self.in_flight_fences[self.current_frame..].ptr);

    const image_index = next_image_result.image_index;

    try self.vkd.resetCommandBuffer(self.command_buffers[self.current_frame], .{});

    try self.recordCommandBuffer(
        self.command_buffers[self.current_frame],
        image_index,
    );

    const wait_semaphores = [_]vk.Semaphore{
        self.image_ready_for_write[self.current_frame],
    };
    const wait_stages = [_]vk.PipelineStageFlags{
        .{
            .color_attachment_output_bit = true,
        },
    };

    const singal_semaphores = [_]vk.Semaphore{self.image_ready_for_present[self.current_frame]};

    const submit_info: vk.SubmitInfo = .{
        .wait_semaphore_count = wait_semaphores.len,
        .p_wait_semaphores = wait_semaphores[0..].ptr,
        .p_wait_dst_stage_mask = wait_stages[0..].ptr,
        .command_buffer_count = 1,
        .p_command_buffers = self.command_buffers[self.current_frame..].ptr,
        .signal_semaphore_count = singal_semaphores.len,
        .p_signal_semaphores = singal_semaphores[0..].ptr,
    };

    try self.vkd.queueSubmit(
        self.graphics_queue,
        1,
        @ptrCast(&submit_info),
        self.in_flight_fences[self.current_frame],
    );

    const swapchains = [_]vk.SwapchainKHR{self.swapchain};

    const present_info: vk.PresentInfoKHR = .{
        .wait_semaphore_count = singal_semaphores.len,
        .p_wait_semaphores = singal_semaphores[0..].ptr,
        .swapchain_count = 1,
        .p_swapchains = swapchains[0..].ptr,
        .p_image_indices = @ptrCast(&image_index),
    };

    const queue_result = try self.vkd.queuePresentKHR(self.present_queue, &present_info);

    switch (queue_result) {
        .error_out_of_date_khr, .suboptimal_khr => {
            self.frame_buffer_resized = false;
            try self.recreateSwapChain();
        },
        else => {
            if (self.frame_buffer_resized) {
                self.frame_buffer_resized = false;
                try self.recreateSwapChain();
            }
        },
    }

    self.current_frame = (self.current_frame + 1) % MAX_FRAMES_IN_FLIGHT;
}

fn updateUniformBuffer(self: *Self, current_image: usize) !void {
    const ubo: UniformBufferObject = .{
        .mvp = self.computeVsParams(0.0, 0.0, 0.0),
    };

    const dest: *UniformBufferObject = @ptrCast(@alignCast(self.uniform_buffers_mapped[current_image].?));
    dest.* = ubo;
}

// Frustum near and far.
pub const near = 0.01;
pub const far = 1000;

pub fn computeVsParams(self: *const Self, rx: f32, ry: f32, rz: f32) zlm.Mat4 {
    const view = zlm.Mat4.createLookAt(
        self.camera_pos,
        self.camera_pos.add(self.camera_front),
        self.camera_up,
    );

    const model = zlm.Mat4.createTranslationXYZ(rx, ry, rz);
    const aspect = @as(f32, @floatFromInt(self.swapchain_extent.width)) //
        / @as(f32, @floatFromInt(self.swapchain_extent.height));
    var proj = zlm.Mat4.createPerspective(zlm.toRadians(45.0), aspect, near, far);

    proj.fields[1][1] *= -1.0;

    const mvp = model.mul(view).mul(proj);
    return mvp;
}

fn playerMovement(self: *Self) !void {
    var cx: f64 = 0.0;
    var cy: f64 = 0.0;
    glfw.getCursorPos(self.window, &cx, &cy);

    if (self.captured_moust) {
        const dx = self.mouseX - cx;
        const dy = self.mouseY - cy;
        self.mouseX = @floatCast(cx);
        self.mouseY = @floatCast(cy);

        self.yaw -= @floatCast(dx * self.sensitivity);
        self.pitch -= @floatCast(dy * self.sensitivity);
    }

    const dt: f32 = @floatCast(@as(f64, @floatFromInt(self.time_diff_ns * 60)) / std.time.ns_per_s);

    if (self.pitch > 89.0) {
        self.pitch = 89.0;
    }
    if (self.pitch > -89.0) {
        self.pitch = -89.0;
    }

    var direction: zlm.Vec3 = undefined;
    direction.x = @cos(zlm.toRadians(self.yaw)) * @cos(zlm.toRadians(self.pitch));
    direction.y = @sin(zlm.toRadians(self.pitch));
    direction.z = @sin(zlm.toRadians(self.yaw)) * @cos(zlm.toRadians(self.pitch));
    self.camera_front = direction.normalize();

    const nonY = zlm.Vec3.new(1.0, 0.0, 1.0);

    //const lookY = mat4.createAngleAxis(Vec3.unitX, self.mouseY);
    self.camera_pos = self.camera_pos.add(self.camera_front.scale(self.dz * dt).mul(nonY));
    self.camera_pos = self.camera_pos.sub(
        self.camera_front.cross(self.camera_up).normalize().scale(self.dx * dt).mul(nonY),
    );

    self.camera_pos = self.camera_pos.add(zlm.Vec3.new(0.0, self.dy * dt, 0.0));

    if (!self.camera_pos.eql(self.prev_camera_pos) or !self.camera_front.eql(self.prev_camera_front) or !self.camera_up.eql(self.prev_camera_up)) {
        //self.selector.calcPos();
        self.prev_camera_pos = self.camera_pos;
        self.prev_camera_front = self.camera_front;
        self.prev_camera_up = self.camera_up;
    }
}

fn recreateSwapChain(self: *Self) !void {
    var width: c_int = 0;
    var height: c_int = 0;

    glfw.getFramebufferSize(self.window, &width, &height);
    while (width == 0 or height == 0) {
        glfw.getFramebufferSize(self.window, &width, &height);
        glfw.waitEvents();
    }

    try self.dev.deviceWaitIdle();

    self.cleanupSwapChain();

    try self.createSwapChain();
    try self.createImageViews();
    try self.createDepthResources();
    try self.createFramebuffers();
}

fn cleanupSwapChain(self: *Self) void {
    self.dev.destroyImageView(self.depth_image_view, null);
    self.dev.destroyImage(self.depth_image, null);
    self.dev.freeMemory(self.depth_image_memory, null);

    for (self.swapchain_framebuffers.items) |framebuffer| {
        self.dev.destroyFramebuffer(framebuffer, null);
    }

    for (self.swapchain_image_views.items) |view| {
        self.dev.destroyImageView(view, null);
    }

    self.vkd.destroySwapchainKHR(self.device, self.swapchain, null);
}

fn cleanup(self: *Self) void {
    state.close.store(true, .release);
    state.workerThreadHandle.join();

    self.cleanupSwapChain();

    var mesh_iter = self.solid_meshes.iterator();
    while (mesh_iter.next()) |mesh| {
        mesh.value_ptr.deinit(
            self.dev,
            self.chunks_descriptor_pool,
        ) catch unreachable;
    }
    mesh_iter = self.transparent_meshes.iterator();
    while (mesh_iter.next()) |mesh| {
        mesh.value_ptr.deinit(
            self.dev,
            self.chunks_descriptor_pool,
        ) catch unreachable;
    }
    self.solid_meshes.deinit();
    self.transparent_meshes.deinit();
    self.meshes_to_regen.deinit();

    state.chunkMap.deinit();
    state.recvChunkMeshQueue.deinit();
    state.genChunkMeshQueue.deinit();
    state.recvWorkerThreadQueue.deinit();
    state.sendWorkerThreadQueue.deinit();
    for (state.blocksArr.items) |*block| {
        block.deinit();
    }
    state.blocksArr.deinit();
    state.chunkGenFuncs.deinit();
    state.blocksNameArr.deinit();
    state.texturesArena.deinit();

    state.chunkPool.deinit();

    self.dev.destroySampler(self.texture_image_sampler, null);
    self.dev.destroyImageView(self.texture_image_view, null);

    self.dev.destroyImage(self.texture_image, null);
    self.dev.freeMemory(self.texture_image_memory, null);

    self.dev.destroyDescriptorPool(self.descriptor_pool, null);
    self.dev.destroyDescriptorPool(self.chunks_descriptor_pool, null);

    self.dev.destroyDescriptorSetLayout(self.descriptor_set_layout, null);
    self.dev.destroyDescriptorSetLayout(self.chunk_descriptor_set_layout, null);

    self.dev.destroyBuffer(self.index_buffer, null);
    self.dev.freeMemory(self.index_buffer_memory, null);

    self.dev.destroyBuffer(self.vertex_buffer, null);
    self.dev.freeMemory(self.vertex_buffer_memory, null);

    self.dev.destroyPipeline(self.graphics_pipeline, null);
    self.dev.destroyPipelineLayout(self.pipeline_layout, null);

    self.dev.destroyRenderPass(self.render_pass, null);

    for (0..MAX_FRAMES_IN_FLIGHT) |i| {
        self.dev.destroySemaphore(self.image_ready_for_present[i], null);
        self.dev.destroySemaphore(self.image_ready_for_write[i], null);
        self.dev.destroyFence(self.in_flight_fences[i], null);
    }

    self.dev.destroyCommandPool(self.command_pool, null);

    self.swapchain_framebuffers.deinit(self.allocator);

    self.swapchain_image_views.deinit(self.allocator);
    self.swapchain_images.deinit(self.allocator);

    self.vkd.destroyDevice(self.device, null);

    if (enable_validation_layers) {
        self.vki.destroyDebugUtilsMessengerEXT(self.instance, self.debugMessenger, null);
    }

    self.vki.destroySurfaceKHR(self.instance, self.surface, null);
    self.vki.destroyInstance(self.instance, null);

    glfw.destroyWindow(self.window);

    glfw.terminate();
}

fn initGame(self: *Self) !void {
    state.allocator = self.allocator;
    state.texturesArena = .init(self.allocator);
    state.textureMap = std.StringHashMap(u32).init(state.texturesArena.allocator());

    state.blocksArr = std.array_list.Managed(blocks.Block).init(state.allocator);
    state.blocksNameArr = std.array_list.Managed(u8).init(state.allocator);

    try state.blocksArr.append(blocks.AirBlock);

    try main.defaultBlocks();

    for (state.blocksArr.items) |block| {
        try state.blocksNameArr.appendSlice(block.blockName.*);
        try state.blocksNameArr.append(0);
    }
    try state.blocksNameArr.append(0);

    const blockTextures = try textures.registerBlocks(state.blocksArr.items);

    defer state.texturesArena.allocator().free(blockTextures);

    main.registerBlockUpdates();

    state.atlas = try textures.createAtlas(blockTextures, state.texturesArena.allocator());

    for (blockTextures, 0..) |blkName, i| {
        const basePath = "assets/textures/";
        const name = try state.texturesArena.allocator().alloc(u8, blkName.len - "assets/textures/".len);
        @memcpy(name, blkName[basePath.len..]);
        std.log.info("Adding texture name: {s}", .{name});
        try state.textureMap.put(name, @intCast(i));
        state.texturesArena.allocator().free(blkName);
    }

    const State = main.State;

    state.genChunkMeshQueue = try State.genChunkQueueT.init(state.allocator, 64 * 64);

    state.sendWorkerThreadQueue = try util.mspc(workerThread.toWorkerThreadMessage) //
        .init(state.allocator, 1024);
    state.recvWorkerThreadQueue = try util.mspc(workerThread.fromWorkerThreadMessage) //
        .init(state.allocator, 1024);

    state.recvChunkMeshQueue = try @TypeOf(state.recvChunkMeshQueue).init(state.allocator, 64 * 64);

    state.chunksInFlightSet = State.chunksInFlightT.init(state.allocator);

    state.chunkMap = std.AutoHashMap(IVec3, chunks.Chunk).init(state.allocator);
    try state.chunkMap.ensureTotalCapacity(32 * 32);

    state.chunksToRegen = zset.ArraySetManaged(IVec3).init(state.allocator);

    state.chunkGenFuncs = std.array_list.Managed(chunks.ChunkGenFunc).init(state.allocator);
    try chunks.add_builtin_gen_funcs();

    state.pass_action.colors[0] = .{
        .load_action = .CLEAR,
        .clear_value = .{ .r = 0.25, .g = 0.5, .b = 0.75, .a = 1 },
    };

    try state.chunkPool.init(.{
        .allocator = state.allocator,
    });

    state.workerThreadHandle = try std.Thread.spawn(
        .{},
        workerThread.workerThread,
        .{},
    );
}

fn beginSingleTimeCommandsGeneric(
    dev: Device,
    command_pool: vk.CommandPool,
) !vk.CommandBuffer {
    const alloc_info: vk.CommandBufferAllocateInfo = .{
        .level = .primary,
        .command_pool = command_pool,
        .command_buffer_count = 1,
    };

    var command_buffer: vk.CommandBuffer = .null_handle;
    try dev.allocateCommandBuffers(&alloc_info, @ptrCast(&command_buffer));

    const begin_info: vk.CommandBufferBeginInfo = .{
        .flags = .{ .one_time_submit_bit = true },
    };

    try dev.beginCommandBuffer(command_buffer, &begin_info);

    return command_buffer;
}

fn endSingleTimeCommandsGeneric(
    dev: Device,
    command_pool: vk.CommandPool,
    queue: vk.Queue,
    command_buffer: vk.CommandBuffer,
) !void {
    try dev.endCommandBuffer(command_buffer);

    const submit_info: vk.SubmitInfo = .{
        .command_buffer_count = 1,
        .p_command_buffers = @ptrCast(&command_buffer),
    };

    try dev.queueSubmit(queue, 1, @ptrCast(&submit_info), .null_handle);
    try dev.queueWaitIdle(queue);

    dev.freeCommandBuffers(command_pool, 1, @ptrCast(&command_buffer));
}

pub fn copyBufferGeneric(
    dev: Device,
    command_pool: vk.CommandPool,
    queue: vk.Queue,
    src_buffer: vk.Buffer,
    dst_buffer: vk.Buffer,
    size: vk.DeviceSize,
) !void {
    const command_buffer = try beginSingleTimeCommandsGeneric(dev, command_pool);

    const copy_region: vk.BufferCopy = .{
        .src_offset = 0,
        .dst_offset = 0,
        .size = size,
    };

    dev.cmdCopyBuffer(command_buffer, src_buffer, dst_buffer, 1, @ptrCast(&copy_region));

    try endSingleTimeCommandsGeneric(dev, command_pool, queue, command_buffer);
}

fn keyCallback(
    window: ?*glfw.Window,
    key: glfw.Key,
    scancode: c_int,
    action: c_int,
    mods: glfw.Modifiers,
) callconv(.c) void {
    _ = scancode; // autofix
    _ = mods; // autofix

    var self: *Self = getFromWindow(window);

    const change: f32 = switch (action) {
        glfw.Press => 1.0,
        glfw.Release => -1.0,
        glfw.Repeat => 0.0,
        else => blk: {
            std.log.err("Unkown action: {d}", .{action});
            break :blk 0.0;
        },
    };

    switch (key) {
        glfw.KeyA => {
            self.dx += change;
            std.log.info("DX: {d}", .{self.dx});
        },
        glfw.KeyD => {
            self.dx -= change;
        },
        glfw.KeyW => {
            self.dz += change;
        },
        glfw.KeyS => {
            self.dz -= change;
        },
        glfw.KeySpace => {
            self.dy += change;
        },
        glfw.KeyLeftShift => {
            self.dy -= change;
        },
        glfw.KeyEscape => {
            if (action == glfw.Press) {
                if (self.captured_moust) {
                    glfw.setInputMode(self.window, c.GLFW_CURSOR, c.GLFW_CURSOR_NORMAL);
                    self.captured_moust = false;
                } else {
                    glfw.setInputMode(self.window, c.GLFW_CURSOR, c.GLFW_CURSOR_DISABLED);
                    self.captured_moust = true;
                }
            }
        },
        glfw.KeyR => {
            std.log.info("Dx: {d}", .{self.dx});
            std.log.info("Dy: {d}", .{self.dy});
            std.log.info("Dz: {d}", .{self.dz});
            self.dx = 0.0;
            self.dy = 0.0;
            self.dz = 0.0;
        },
        else => {},
    }
}

fn getFromWindow(window: ?*glfw.Window) *Self {
    const self: *Self = @ptrCast(@alignCast(glfw.getWindowUserPointer(window).?));
    return self;
}

fn renderDistanceGen(self: *Self) !void {
    const chunkPos = chunks.worldToChunkPos(self.camera_pos).chunkPos;
    const renderDistance2: u32 = @as(u32, @intCast(state.renderDistance)) * @as(u32, @intCast(state.renderDistance));

    for (0..(state.renderDistance + 2) * 2) |dx| {
        pos: for (0..(state.renderDistance + 2) * 2) |dz| {
            const toGenPos = IVec3.new(
                @as(i64, @intCast(dx)) - state.renderDistance + chunkPos.x,
                0,
                @as(i64, @intCast(dz)) - state.renderDistance + chunkPos.z,
            );

            if (toGenPos.distance2(chunkPos) > renderDistance2) {
                continue;
            }

            if (state.chunkMap.contains(toGenPos)) {
                for ([_]*const MeshMap{ &self.solid_meshes, &self.transparent_meshes }) |mesh_map| {
                    if (mesh_map.contains(toGenPos)) {
                        continue :pos;
                    }
                }
                try chunks.mark_chunk_for_regen(toGenPos);
                continue :pos;
            }

            if (state.chunksInFlightSet.get(toGenPos) == null) {
                try state.sendWorkerThreadQueue.enqueue(.{
                    .GetChunk = toGenPos,
                });

                try state.chunksInFlightSet.put(toGenPos, .{});
            }
        }
    }
}
