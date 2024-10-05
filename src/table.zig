const std = @import("std");
const memory = @import("memory.zig");
const grow_capacity = @import("list.zig").grow_capacity;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;

pub const Table = struct {
    const Self = @This();
    const MAX_LOAD = 0.75;

    entries: []Entry,
    count: usize,

    pub fn init() Self {
        return .{ .entries = &[_]Entry{}, .count = 0 };
    }

    pub fn deinit(self: *Self) void {
        memory.free(self.entries);
        self.* = init();
    }

    pub fn get(self: *Self, key: *ObjString) ?Value {
        if (self.count == 0) return null;
        const entry = find_entry(self.entries, key);
        if (entry.key == null) return null;
        return entry.value;
    }

    pub fn insert(self: *Self, key: *ObjString, value: Value) !bool {
        if (self.count + 1 > self.max_size()) {
            const new_capacity = grow_capacity(self.entries.len);
            try self.adjust_capacity(new_capacity);
        }
        const entry = find_entry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value == .nil) self.count += 1;
        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn delete(self: *Self, key: *ObjString) bool {
        if (self.count == 0) return false;
        const entry = find_entry(self.entries, key);
        if (entry.key == null) return false;
        // Tombstone
        entry.key = null;
        entry.value = Value{ .bool_ = true };
        return true;
    }

    pub fn find_key(self: *const Self, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;
        const capacity = self.entries.len;
        var index = hash % capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.value.as_slice(), chars))
                    return key;
            } else {
                // stop if we find an empty non-tombstone entry.
                if (entry.value == .nil) return null;
            }
            index = (index + 1) % capacity;
        }
    }

    fn find_entry(entries: []Entry, key: *ObjString) *Entry {
        const capacity = entries.len;
        var index = key.hash % capacity;
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
            index = (index + 1) % capacity;
        }
    }

    fn copy_all(self: *const Self, other: *Self) !void {
        for (self.entries) |entry| {
            if (entry.key) |key|
                try other.insert(key, entry.value);
        }
    }

    fn adjust_capacity(self: *Self, new_capacity: usize) !void {
        const slice: []Entry = &[_]Entry{};
        const new_entries = try memory.reallocate(slice, new_capacity);
        for (new_entries) |*entry| {
            entry.key = null;
            entry.value = Value.nil;
        }
        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key == null) continue;
            const key = entry.key.?;
            const dest = find_entry(new_entries, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        }
        memory.free(self.entries);
        self.entries = new_entries;
    }

    inline fn max_size(self: *const Self) usize {
        const capacity: f64 = @floatFromInt(self.entries.len);
        return @intFromFloat(MAX_LOAD * capacity);
    }
};

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};
