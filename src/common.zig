const std = @import("std");

pub const UINT8_COUNT = std.math.maxInt(u8) + 1;

pub inline fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}
