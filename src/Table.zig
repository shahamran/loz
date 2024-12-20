const std = @import("std");
const common = @import("common.zig");
const Obj = @import("Obj.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

const Self = @This();
const MAX_LOAD = 0.75;

allocator: std.mem.Allocator,
entries: []Entry,
count: usize,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .entries = &[_]Entry{},
        .count = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.entries);
    self.* = init(self.allocator);
}

pub fn get(self: *Self, key: *Obj.String) ?Value {
    if (self.count == 0) return null;
    const entry = findEntry(self.entries, key);
    if (entry.key == null) return null;
    return entry.value;
}

pub fn insert(self: *Self, key: *Obj.String, value: Value) !bool {
    if (self.count + 1 > self.maxSize()) {
        const new_capacity = common.grow_capacity(self.entries.len);
        try self.adjustCapacity(new_capacity);
    }
    const entry = findEntry(self.entries, key);
    const is_new_key = entry.key == null;
    if (is_new_key and entry.value == .nil) self.count += 1;
    entry.key = key;
    entry.value = value;
    return is_new_key;
}

pub fn delete(self: *Self, key: *Obj.String) bool {
    if (self.count == 0) return false;
    const entry = findEntry(self.entries, key);
    if (entry.key == null) return false;
    entry.* = Entry.tombstone();
    return true;
}

pub fn findKey(self: *const Self, chars: []const u8, hash: u32) ?*Obj.String {
    if (self.count == 0) return null;
    const capacity = self.entries.len;
    var index = hash & (capacity - 1);
    while (true) {
        const entry = &self.entries[index];
        if (entry.key) |key| {
            if (key.hash == hash and std.mem.eql(u8, key.value.asSlice(), chars))
                return key;
        } else if (entry.value == .nil) {
            // stop if we find an empty non-tombstone entry.
            return null;
        }
        index = (index + 1) & (capacity - 1);
    }
}

fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
    const capacity = entries.len;
    var index = key.hash & (capacity - 1);
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &entries[index];
        if (entry.key) |existing| {
            if (existing == key) return entry;
        } else {
            // entry.key is null
            if (entry.value == .nil) {
                // Empty entry -> return the first tombstone or the empty entry
                return tombstone orelse entry;
            } else {
                if (tombstone == null) tombstone = entry;
            }
        }
        index = (index + 1) & (capacity - 1);
    }
}

pub fn copyAll(self: *const Self, other: *Self) !void {
    for (self.entries) |entry| {
        if (entry.key) |key|
            _ = try other.insert(key, entry.value);
    }
}

fn adjustCapacity(self: *Self, new_capacity: usize) !void {
    const new_entries = try self.allocator.alloc(Entry, new_capacity);
    for (new_entries) |*entry| {
        entry.key = null;
        entry.value = Value.nil;
    }
    self.count = 0;
    for (self.entries) |entry| {
        if (entry.key == null) continue;
        const key = entry.key.?;
        const dest = findEntry(new_entries, key);
        dest.key = key;
        dest.value = entry.value;
        self.count += 1;
    }
    self.allocator.free(self.entries);
    self.entries = new_entries;
}

inline fn maxSize(self: *const Self) usize {
    const capacity: f64 = @floatFromInt(self.entries.len);
    return @intFromFloat(MAX_LOAD * capacity);
}

pub const Entry = struct {
    key: ?*Obj.String,
    value: Value,

    fn tombstone() Entry {
        return .{ .key = null, .value = .{ .bool_ = true } };
    }
};
