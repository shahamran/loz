const List = @import("list.zig").List;

/// Null terminated list of bytes, dynamically sized.
pub const String = struct {
    const Self = @This();

    chars: List(u8),

    pub fn init() Self {
        return .{
            .chars = List(u8).init(),
        };
    }

    pub fn init_from(slice: []const u8) !Self {
        var string = Self.init();
        try string.append(slice);
        return string;
    }

    pub fn deinit(self: *Self) void {
        self.chars.deinit();
        self.* = init();
    }

    pub fn str_len(self: *const Self) usize {
        return self.as_slice().len;
    }

    pub fn clone(self: *const Self) !Self {
        var new_string = Self.init();
        try new_string.append(self.as_slice());
        return new_string;
    }

    pub fn as_slice(self: *const Self) [:0]const u8 {
        if (self.chars.items.len == 0) return "";
        return self.chars.items[0 .. self.chars.items.len - 1 :0];
    }

    /// Append the given chars to the end of this string.
    pub fn append(self: *Self, str: []const u8) !void {
        if (str.len == 0) return;
        if (self.chars.items.len > 0) {
            try self.chars.reserve(self.chars.items.len + str.len);
            _ = self.chars.pop(); // remove null terminator
        } else {
            try self.chars.reserve(str.len + 1);
        }
        for (str) |c| self.chars.push(c) catch unreachable;
        self.chars.push(0) catch unreachable; // add null terminator
    }
};

test "string" {
    const expect = @import("std").testing.expect;
    const expectEqualSentinel = @import("std").testing.expectEqualSentinel;
    var string = String.init();
    defer string.deinit();
    try expectEqualSentinel(u8, 0, "", string.as_slice());
    try expect(string.str_len() == 0);
    try expect(string.chars.capacity == 0);
    try string.append("hello");
    try expectEqualSentinel(u8, 0, "hello", string.as_slice());
    try expect(string.str_len() == 5);
}
