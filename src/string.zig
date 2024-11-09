const std = @import("std");
const Allocator = std.mem.Allocator;

/// Null terminated list of bytes, dynamically sized.
pub const String = struct {
    const Self = @This();

    chars: []u8 = &[_]u8{},

    pub fn initFrom(allocator: Allocator, slice: []const u8) !Self {
        const chars = try allocator.alloc(u8, slice.len + 1);
        std.mem.copyForwards(u8, chars, slice);
        chars[slice.len] = 0; // null terminator
        return .{ .chars = chars };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.chars);
    }

    pub fn str_len(self: *const Self) usize {
        return self.chars.len -| 1;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !Self {
        return try initFrom(allocator, self.asSlice());
    }

    pub fn asSlice(self: *const Self) [:0]const u8 {
        if (self.chars.len == 0) return "";
        return self.chars[0 .. self.chars.len - 1 :0];
    }

    /// Append the given chars to the end of this string.
    pub fn append(self: *Self, allocator: Allocator, str: []const u8) !void {
        if (str.len == 0) return;
        var start: usize = undefined;
        var new_n: usize = undefined;
        if (self.chars.len > 0) {
            start = self.chars.len - 1; // override null terminator
            new_n = self.chars.len + str.len;
        } else {
            start = 0;
            new_n = str.len + 1;
        }
        self.chars = try allocator.realloc(self.chars, new_n);
        std.mem.copyForwards(u8, self.chars[start..], str);
        self.chars[self.chars.len - 1] = 0;
    }
};

test "string" {
    const expect = std.testing.expect;
    const expectEqualSentinel = std.testing.expectEqualSentinel;
    const allocator = std.testing.allocator;
    var string = String{};
    defer string.deinit(allocator);
    try expectEqualSentinel(u8, 0, "", string.asSlice());
    try expect(string.str_len() == 0);
    try string.append(allocator, "hello");
    try expectEqualSentinel(u8, 0, "hello", string.asSlice());
    try expect(string.str_len() == 5);
}
