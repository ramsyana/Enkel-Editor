//! Enkel - Ultra-Minimalist Terminal Editor for IoT Devices
//! Version: 0.0.1
//!
//! MIT License
//! Copyright (c) 2024 Ramsyana <ramsyana@mac.com>
//!
//! Permission is hereby granted, free of charge, to any person obtaining a copy
//! of this software and associated documentation files (the "Software"), to deal
//! in the Software without restriction, including without limitation the rights
//! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//! copies of the Software, and to permit persons to whom the Software is
//! furnished to do so, subject to the following conditions:
//!
//! The above copyright notice and this permission notice shall be included in all
//! copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//! SOFTWARE.
//!

const std = @import("std");
const c = @cImport({
    @cInclude("termios.h");
    @cInclude("stdlib.h");
    @cInclude("stdio.h");
    @cInclude("stdint.h");
    @cInclude("errno.h");
    @cInclude("string.h");
    @cInclude("ctype.h");
    @cInclude("time.h");
    @cInclude("sys/types.h");
    @cInclude("sys/ioctl.h");
    @cInclude("sys/time.h");
    @cInclude("unistd.h");
    @cInclude("fcntl.h");
    @cInclude("signal.h");
});

const ENKEL_VERSION = "0.0.1";

const HL_NORMAL: u8 = 0;
const HL_NONPRINT: u8 = 1;
const HL_COMMENT: u8 = 2;
const HL_MLCOMMENT: u8 = 3;
const HL_KEYWORD1: u8 = 4;
const HL_KEYWORD2: u8 = 5;
const HL_STRING: u8 = 6;
const HL_NUMBER: u8 = 7;
const HL_MATCH: u8 = 8;

const HL_HIGHLIGHT_STRINGS: u32 = 1 << 0;
const HL_HIGHLIGHT_NUMBERS: u32 = 1 << 1;

const KEY_NULL: i32 = 0;
const CTRL_C: i32 = 3;
const CTRL_D: i32 = 4;
const CTRL_F: i32 = 6;
const CTRL_H: i32 = 8;
const TAB: i32 = 9;
const CTRL_L: i32 = 12;
const ENTER: i32 = 13;
const CTRL_Q: i32 = 17;
const CTRL_S: i32 = 19;
const CTRL_U: i32 = 21;
const ESC: i32 = 27;
const BACKSPACE: i32 = 127;
const ARROW_LEFT: i32 = 1000;
const ARROW_RIGHT: i32 = 1001;
const ARROW_UP: i32 = 1002;
const ARROW_DOWN: i32 = 1003;
const DEL_KEY: i32 = 1004;
const HOME_KEY: i32 = 1005;
const END_KEY: i32 = 1006;
const PAGE_UP: i32 = 1007;
const PAGE_DOWN: i32 = 1008;

const AppendBuffer = struct {
    buf: []u8,
    len: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AppendBuffer {
        return .{
            .buf = &[_]u8{},
            .len = 0,
            .allocator = allocator,
        };
    }

    pub fn append(self: *AppendBuffer, str: []const u8) !void {
        if (str.len == 0) return;
        const new_len = self.len + str.len;
        const new_buf = try self.allocator.realloc(self.buf, new_len);
        self.buf = new_buf;
        @memcpy(self.buf[self.len..], str);
        self.len = new_len;
    }

    pub fn deinit(self: *AppendBuffer) void {
        self.allocator.free(self.buf);
    }
};

const HLDB: [1]EditorSyntax = .{
    .{
        .filematch = &[_][]const u8{ ".c", ".h", ".cpp", ".hpp", ".cc" },
        .keywords = &[_][]const u8{ "auto", "break", "case", "continue", "default", "do", "else", "enum", "extern", "for", "goto", "if", "register", "return", "sizeof", "static", "struct", "switch", "typedef", "union", "volatile", "while", "NULL", "alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor", "class", "compl", "constexpr", "const_cast", "deltype", "delete", "dynamic_cast", "explicit", "export", "false", "friend", "inline", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected", "public", "reinterpret_cast", "static_assert", "static_cast", "template", "this", "thread_local", "throw", "true", "try", "typeid", "typename", "virtual", "xor", "xor_eq", "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|", "short|", "auto|", "const|", "bool|" },
        .singleline_comment_start = [_]u8{ '/', '/' },
        .multiline_comment_start = [_]u8{ '/', '*', 0 },
        .multiline_comment_end = [_]u8{ '*', '/', 0 },
        .flags = HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_NUMBERS,
    },
};

const HLDB_ENTRIES = HLDB.len;

pub const EditorSyntax = struct {
    filematch: []const []const u8,
    keywords: []const []const u8,
    singleline_comment_start: [2]u8,
    multiline_comment_start: [3]u8,
    multiline_comment_end: [3]u8,
    flags: u32,
};

pub const EditorRow = struct {
    idx: usize,
    size: usize,
    rsize: usize,
    chars: []u8,
    render: []u8,
    hl: []u8,
    hl_oc: bool,

    pub fn deinit(self: *EditorRow, allocator: std.mem.Allocator) void {
        allocator.free(self.chars);
        allocator.free(self.render);
        allocator.free(self.hl);
    }
};

pub const Editor = struct {
    cx: usize = 0,
    cy: usize = 0,
    rowoff: usize = 0,
    coloff: usize = 0,
    screenrows: usize = 0,
    screencols: usize = 0,
    numrows: usize = 0,
    rawmode: bool = false,
    quit_times: u8 = 2,
    row: []EditorRow = &[_]EditorRow{},
    dirty: bool = false,
    filename: ?[]const u8 = null,
    statusmsg: [80]u8 = [_]u8{0} ** 80,
    statusmsg_time: i64 = 0,
    syntax: ?*const EditorSyntax = null,

    allocator: std.mem.Allocator,

    pub fn deinit(self: *Editor) void {
        if (self.rawmode) {
            self.disableRawMode();
        }
        self.allocator.free(self.filename orelse "");
        for (self.row) |*row| {
            row.deinit(self.allocator);
        }
        self.allocator.free(self.row);
    }
    pub fn setStatusMessage(self: *Editor, comptime format: []const u8, args: anytype) void {
        @memset(self.statusmsg[0..], 0);
        _ = std.fmt.bufPrint(self.statusmsg[0..], format, args) catch |err| {
            std.debug.print("Error in setStatusMessage: {any}\n", .{err});
            return;
        };
        self.statusmsg_time = @as(i64, @truncate(std.time.nanoTimestamp()));
    }

    pub fn enableRawMode(self: *Editor) !void {
        if (self.rawmode) return;
        if (c.isatty(std.posix.STDIN_FILENO) == 0) return error.NotATTY;
        var orig_termios: c.termios = undefined;
        if (c.tcgetattr(std.posix.STDIN_FILENO, &orig_termios) == -1)
            return error.TermiosGetError;

        var raw: c.termios = orig_termios;
        raw.c_iflag &= ~@as(c_ulong, c.BRKINT);
        raw.c_iflag &= ~@as(c_ulong, c.ICRNL);
        raw.c_iflag &= ~@as(c_ulong, c.INPCK);
        raw.c_iflag &= ~@as(c_ulong, c.ISTRIP);
        raw.c_iflag &= ~@as(c_ulong, c.IXON);
        raw.c_oflag &= ~@as(c_ulong, c.OPOST);
        raw.c_cflag |= c.CS8;
        raw.c_lflag &= ~@as(c_ulong, c.ECHO);
        raw.c_lflag &= ~@as(c_ulong, c.ICANON);
        raw.c_lflag &= ~@as(c_ulong, c.IEXTEN);
        raw.c_lflag &= ~@as(c_ulong, c.ISIG);
        raw.c_cc[c.VMIN] = 1;
        raw.c_cc[c.VTIME] = 0;
        if (c.tcsetattr(std.posix.STDIN_FILENO, c.TCSAFLUSH, &raw) < 0) return error.TermiosSetError;
        self.rawmode = true;
        return;
    }

    pub fn disableRawMode(self: *Editor) void {
        if (self.rawmode) {
            var orig_termios: c.termios = undefined;
            switch (c.tcgetattr(c.STDIN_FILENO, &orig_termios)) {
                0 => {},
                else => |err| {
                    std.debug.print("Error in disableRawMode: {any}\n", .{err});
                    return;
                },
            }
            _ = c.tcsetattr(c.STDIN_FILENO, c.TCSAFLUSH, &orig_termios);
            self.rawmode = false;
        }
    }

    pub fn readKey(_: *Editor) !i32 {
        var input_char: u8 = 0;
        const nread = try std.posix.read(std.posix.STDIN_FILENO, @as(*[1]u8, &input_char)[0..]);
        if (nread <= 0) {
            if (nread == 0) {
                return KEY_NULL;
            }
            const err = nread;
            std.debug.print("Read returned -1: {any}\n", .{std.posix.errno(err)});
            std.process.exit(1);
        }

        if (input_char == ESC) {
            var seq: [3]u8 = undefined;
            if (c.read(c.STDIN_FILENO, &seq[0], 1) <= 0) return ESC;
            if (c.read(c.STDIN_FILENO, &seq[1], 1) <= 0) return ESC;
            if (seq[0] != '[' and seq[0] != 'O') return ESC;

            if (seq[0] == '[') {
                if (seq[1] >= '0' and seq[1] <= '9') {
                    if (c.read(c.STDIN_FILENO, &seq[2], 1) <= 0) return ESC;
                    if (seq[2] == '~') {
                        switch (seq[1]) {
                            '3' => return DEL_KEY,
                            '5' => return PAGE_UP,
                            '6' => return PAGE_DOWN,
                            else => return ESC,
                        }
                    }
                }
                switch (seq[1]) {
                    'A' => return ARROW_UP,
                    'B' => return ARROW_DOWN,
                    'C' => return ARROW_RIGHT,
                    'D' => return ARROW_LEFT,
                    'H' => return HOME_KEY,
                    'F' => return END_KEY,
                    else => return ESC,
                }
            } else if (seq[0] == 'O') {
                switch (seq[1]) {
                    'H' => return HOME_KEY,
                    'F' => return END_KEY,
                    else => return ESC,
                }
            }
            return ESC;
        }

        return @intCast(input_char);
    }

    pub fn getCursorPosition(self: *Editor) !void {
        var buf: [32]u8 = undefined;
        var i: usize = 0;

        const written = try std.posix.write(c.STDOUT_FILENO, "\x1b[6n");
        if (written != 4) return error.WriteError;

        while (i < buf.len - 1) {
            const read_res = c.read(c.STDIN_FILENO, &buf[i], 1);
            if (read_res != 1) {
                return error.ReadError;
            }
            if (buf[i] == 'R') {
                i += 1;
                break;
            }
            i += 1;
        }

        buf[i] = 0;

        if (buf[0] != ESC or buf[1] != '[') return error.BadResponse;

        const separator_pos = std.mem.indexOf(u8, buf[2..], ";") orelse return error.BadResponse;
        const rows = try std.fmt.parseInt(c_int, buf[2..][0..separator_pos], 10);
        const cols_end = std.mem.indexOf(u8, buf[2..][separator_pos + 1 ..], "R") orelse return error.BadResponse;
        const cols = try std.fmt.parseInt(c_int, buf[2..][separator_pos + 1 ..][0..cols_end], 10);

        self.cy = @intCast(rows - 1);
        self.cx = @intCast(cols - 1);
    }

    pub fn getWindowSize(self: *Editor) !void {
        var ws: c.winsize = undefined;
        const ioctl_res = c.ioctl(std.posix.STDOUT_FILENO, c.TIOCGWINSZ, &ws);

        if (ioctl_res == -1 or ws.ws_col == 0) {
            var orig_row: usize = 0;
            var orig_col: usize = 0;

            try self.getCursorPosition();
            orig_row = self.cy;
            orig_col = self.cx;

            const written = try std.posix.write(c.STDOUT_FILENO, "\x1b[999C\x1b[999B");
            if (written != 12) return error.WriteError;

            try self.getCursorPosition();
            self.screenrows = self.cy + 1;
            self.screencols = self.cx + 1;

            self.cy = orig_row;
            self.cx = orig_col;
        } else {
            self.screencols = @as(usize, ws.ws_col);
            self.screenrows = if (ws.ws_row > 2) @as(usize, ws.ws_row - 2) else 0;
        }
    }

    fn isSeparator(_: *Editor, char: u8) bool {
        return char == 0 or std.ascii.isWhitespace(char) or std.mem.indexOfScalar(u8, ",.()+-/*=~%[];", char) != null;
    }

    fn editorRowHasOpenComment(_: *Editor, row: *EditorRow) bool {
        if (row.hl.len > 0 and row.rsize > 0 and row.hl[row.rsize - 1] == HL_MLCOMMENT and (row.rsize < 2 or (row.render[row.rsize - 2] != '*' or row.render[row.rsize - 1] != '/'))) {
            return true;
        }
        return false;
    }

    pub fn refreshScreen(self: *Editor) !void {
        var ab = AppendBuffer.init(self.allocator);
        defer ab.deinit();

        try ab.append("\x1b[?25l");
        try ab.append("\x1b[H");
        try ab.append("\x1b[2J");

        try self.drawRows(&ab);
        try self.drawStatusBar(&ab);
        try self.drawMessageBar(&ab);

        var buf: [32]u8 = undefined;
        const cursor_pos = try std.fmt.bufPrint(&buf, "\x1b[{d};{d}H", .{
            self.cy + 1,
            self.cx + 1,
        });
        try ab.append(cursor_pos);

        try ab.append("\x1b[?25h");

        _ = try std.posix.write(std.posix.STDOUT_FILENO, ab.buf[0..ab.len]);
    }

    fn drawRows(self: *Editor, ab: *AppendBuffer) !void {
        var y: usize = 0;
        while (y < self.screenrows) : (y += 1) {
            try ab.append("\x1b[K");
            const filerow = y + self.rowoff;
            if (filerow >= self.numrows) {
                if (self.numrows == 0 and y == self.screenrows / 3) {
                    const welcome = ".:! Enkel v" ++ ENKEL_VERSION ++ " - The Ultra-Minimalist Terminal Editor for IoT Devices !:.";
                    const padding = (self.screencols - welcome.len) / 2;
                    if (padding > 0) {
                        try ab.append("~");

                        var i: usize = 1;
                        while (i < padding) : (i += 1) {
                            try ab.append(" ");
                        }
                    }
                    try ab.append(welcome);
                } else {
                    try ab.append("~");
                }
            } else {
                const row = &self.row[filerow];
                var len = row.rsize - self.coloff;
                if (len < 0) len = 0;
                if (len > self.screencols) len = self.screencols;
                if (len > 0) {
                    try ab.append(row.render[self.coloff..][0..len]);
                }
            }

            if (y < self.screenrows - 1) {
                try ab.append("\r\n");
            }
        }
    }

    pub fn updateSyntax(self: *Editor, row: *EditorRow) !void {
        row.hl = try self.allocator.realloc(row.hl, row.rsize);
        @memset(row.hl[0..], HL_NORMAL);

        if (self.syntax == null) return;

        var i: usize = 0;
        var prev_sep: bool = true;
        var in_string: u8 = 0;
        var in_comment: bool = false;
        var p: usize = 0;

        while (p < row.render.len and std.ascii.isWhitespace(row.render[p])) {
            p += 1;
            i += 1;
        }

        if (row.idx > 0 and self.editorRowHasOpenComment(&self.row[row.idx - 1])) {
            in_comment = true;
        }
        while (p < row.render.len) {
            if (prev_sep and row.render[p] == self.syntax.?.singleline_comment_start[0] and row.render[p + 1] == self.syntax.?.singleline_comment_start[1]) {
                @memset(row.hl[i..], HL_COMMENT);
                return;
            }

            if (in_comment) {
                row.hl[i] = HL_MLCOMMENT;
                if (row.render[p] == self.syntax.?.multiline_comment_end[0] and row.render[p + 1] == self.syntax.?.multiline_comment_end[1]) {
                    row.hl[i + 1] = HL_MLCOMMENT;
                    p += 2;
                    i += 2;
                    in_comment = false;
                    prev_sep = true;
                    continue;
                } else {
                    prev_sep = false;
                    p += 1;
                    i += 1;
                    continue;
                }
            } else if (row.render[p] == self.syntax.?.multiline_comment_start[0] and row.render[p + 1] == self.syntax.?.multiline_comment_start[1]) {
                row.hl[i] = HL_MLCOMMENT;
                row.hl[i + 1] = HL_MLCOMMENT;
                p += 2;
                i += 2;
                in_comment = true;
                prev_sep = false;
                continue;
            }
            if (in_string > 0) {
                row.hl[i] = HL_STRING;
                if (row.render[p] == '\\') {
                    row.hl[i + 1] = HL_STRING;
                    p += 2;
                    i += 2;
                    prev_sep = false;
                    continue;
                }
                if (row.render[p] == in_string) {
                    in_string = 0;
                }
                p += 1;
                i += 1;
                continue;
            } else {
                if (row.render[p] == '"' or row.render[p] == '\'') {
                    in_string = row.render[p];
                    row.hl[i] = HL_STRING;
                    p += 1;
                    i += 1;
                    prev_sep = false;
                    continue;
                }
            }
            if (!std.ascii.isPrint(row.render[p])) {
                row.hl[i] = HL_NONPRINT;
                p += 1;
                i += 1;
                prev_sep = false;
                continue;
            }
            if ((std.ascii.isDigit(row.render[p]) and (prev_sep or row.hl[i - 1] == HL_NUMBER)) or (row.render[p] == '.' and i > 0 and row.hl[i - 1] == HL_NUMBER)) {
                row.hl[i] = HL_NUMBER;
                p += 1;
                i += 1;
                prev_sep = false;
                continue;
            }

            if (prev_sep) {
                for (self.syntax.?.keywords) |keyword| {
                    if (std.mem.eql(u8, row.render[p..], keyword) and self.isSeparator(row.render[p + keyword.len])) {
                        @memset(row.hl[i..][0..keyword.len], if (keyword[keyword.len - 1] == '|') HL_KEYWORD2 else HL_KEYWORD1);
                        p += keyword.len;
                        i += keyword.len;
                        break;
                    }
                }
                prev_sep = false;
                continue;
            }
            prev_sep = self.isSeparator(row.render[p]);
            p += 1;
            i += 1;
        }
        const oc = self.editorRowHasOpenComment(row);
        if (row.hl_oc != oc and row.idx + 1 < self.numrows) try self.updateSyntax(&self.row[row.idx + 1]);
        row.hl_oc = oc;
    }

    pub fn editorSyntaxToColor(_: *Editor, hl: u8) i32 {
        switch (hl) {
            HL_COMMENT, HL_MLCOMMENT => return 36,
            HL_KEYWORD1 => return 33,
            HL_KEYWORD2 => return 32,
            HL_STRING => return 35,
            HL_NUMBER => return 31,
            HL_MATCH => return 34,
            else => return 37,
        }
    }

    pub fn selectSyntaxHighlight(self: *Editor, filename: []const u8) !void {
        for (HLDB) |syntax| {
            for (syntax.filematch) |pat| {
                if (pat.len == 0) continue;
                const patlen = pat.len;
                if (std.mem.indexOf(u8, filename, pat)) |match_pos| {
                    if (pat[0] != '.' or filename.len == match_pos + patlen) {
                        self.syntax = &syntax;
                        return;
                    }
                }
            }
        }
    }

    pub fn updateRow(self: *Editor, row: *EditorRow) void {
        var tabs: usize = 0;
        for (row.chars[0..row.size]) |ch| {
            if (ch == TAB) tabs += 1;
        }

        self.allocator.free(row.render);
        const allocsize = row.size + tabs * 7;
        row.render = self.allocator.alloc(u8, allocsize) catch unreachable;

        var idx: usize = 0;
        for (row.chars[0..row.size]) |ch| {
            if (ch == TAB) {
                row.render[idx] = ' ';
                idx += 1;
                while (idx % 8 != 0 and idx < allocsize) {
                    row.render[idx] = ' ';
                    idx += 1;
                }
            } else {
                row.render[idx] = ch;
                idx += 1;
            }
        }
        row.rsize = idx;
        self.updateSyntax(row) catch {};
    }

    pub fn insertRow(self: *Editor, at: usize, s: []const u8) !void {
        if (at > self.numrows) return error.InvalidPosition;

        var new_rows = try self.allocator.alloc(EditorRow, self.numrows + 1);
        errdefer self.allocator.free(new_rows);

        var new_row = EditorRow{
            .idx = at,
            .size = s.len,
            .rsize = 0,
            .chars = try self.allocator.alloc(u8, s.len),
            .render = &[_]u8{},
            .hl = &[_]u8{},
            .hl_oc = false,
        };

        @memcpy(new_row.chars[0..], s[0..]);

        if (at > 0) {
            @memcpy(new_rows[0..at], self.row[0..at]);
        }

        new_rows[at] = new_row;

        if (at < self.numrows) {
            @memcpy(new_rows[at + 1 ..], self.row[at..]);
            for (new_rows[at + 1 ..]) |*row| {
                row.idx += 1;
            }
        }

        self.allocator.free(self.row);
        self.row = new_rows;
        self.numrows += 1;

        self.updateRow(&self.row[at]);
        self.dirty = true;
    }

    fn freeRow(self: *Editor, row: *EditorRow) void {
        row.deinit(self.allocator);
    }

    pub fn delRow(self: *Editor, at: usize) void {
        if (at >= self.numrows) return;
        self.freeRow(&self.row[at]);

        if (self.numrows <= 1) {
            self.allocator.free(self.row);
            self.row = &[_]EditorRow{};
            self.numrows = 0;
            return;
        }

        if (at < self.numrows - 1) {
            var i: usize = at;
            while (i < self.numrows - 1) : (i += 1) {
                self.row[i] = self.row[i + 1];
                self.row[i].idx = i;
            }
        }

        if (self.allocator.realloc(self.row, self.numrows - 1)) |new_rows| {
            self.row = new_rows;
            self.numrows -= 1;
            self.dirty = true;
        } else |_| {
            return;
        }
    }

    pub fn rowsToString(self: *Editor, allocator: std.mem.Allocator) ![]u8 {
        var totlen: usize = 0;
        for (self.row) |row| {
            totlen += row.size + 1;
        }

        var buf = try allocator.alloc(u8, totlen);
        errdefer allocator.free(buf);

        var p: usize = 0;
        for (self.row) |row| {
            if (p + row.size > buf.len) return error.BufferTooSmall;
            @memcpy(buf[p..][0..row.size], row.chars[0..row.size]);
            p += row.size;
            if (p < buf.len) {
                buf[p] = '\n';
                p += 1;
            }
        }
        return buf;
    }

    pub fn rowInsertChar(self: *Editor, row: *EditorRow, at: usize, char: u8) !void {
        if (at > row.size) {
            const padlen = at - row.size;
            row.chars = try self.allocator.realloc(row.chars, row.size + padlen + 2);
            @memset(row.chars[row.size..], ' ');
            row.chars[row.size + padlen] = @intCast(0);
            row.size += padlen + 1;
        } else {
            row.chars = try self.allocator.realloc(row.chars, row.size + 2);
            std.mem.copyBackwards(u8, row.chars[at + 1 .. row.size + 1], row.chars[at..row.size]);
            row.size += 1;
        }
        row.chars[at] = char;

        self.updateRow(row);
        self.dirty = true;
    }

    pub fn rowAppendString(self: *Editor, row: *EditorRow, s: []const u8) !void {
        const new_size = row.size + s.len;
        const new_chars = try self.allocator.realloc(row.chars, new_size);

        @memcpy(new_chars[row.size..new_size], s);

        row.chars = new_chars;
        row.size = new_size;

        self.updateRow(row);
        self.dirty = true;
    }

    pub fn rowDelChar(self: *Editor, row: *EditorRow, at: usize) void {
        if (at >= row.size or row.size == 0) return;

        const new_size = row.size - 1;
        if (new_size == 0) {
            self.allocator.free(row.chars);
            row.chars = &[_]u8{};
            row.size = 0;
            self.updateRow(row);
            self.dirty = true;
            return;
        }

        const new_chars = self.allocator.alloc(u8, new_size) catch return;

        if (at > 0) {
            @memcpy(new_chars[0..at], row.chars[0..at]);
        }

        if (at < new_size) {
            @memcpy(new_chars[at..new_size], row.chars[at + 1 .. row.size]);
        }

        self.allocator.free(row.chars);
        row.chars = new_chars;
        row.size = new_size;

        self.updateRow(row);
        self.dirty = true;
    }

    pub fn insertChar(self: *Editor, char: u8) !void {
        const filerow = self.rowoff + self.cy;
        const filecol = self.coloff + self.cx;
        const row = if (filerow >= self.numrows) null else &self.row[filerow];

        if (row == null) {
            while (self.numrows <= filerow) {
                try self.insertRow(self.numrows, "");
            }
        }
        const row_2 = &self.row[filerow];
        try self.rowInsertChar(row_2, filecol, char);

        if (self.screencols > 0 and self.cx == self.screencols - 1) {
            self.coloff += 1;
        } else {
            self.cx += 1;
        }
        self.dirty = true;
    }

    pub fn insertNewline(self: *Editor) !void {
        const filerow = self.rowoff + self.cy;
        const filecol = self.coloff + self.cx;

        if (filerow >= self.numrows) {
            try self.insertRow(filerow, "");
        } else {
            var row = &self.row[filerow];

            if (filecol >= row.size) {
                try self.insertRow(filerow + 1, "");
            } else {
                const new_row_content = try self.allocator.dupe(u8, row.chars[filecol..row.size]);
                defer self.allocator.free(new_row_content);

                const new_chars = try self.allocator.realloc(row.chars, filecol);
                row.chars = new_chars;
                row.size = filecol;
                self.updateRow(row);

                try self.insertRow(filerow + 1, new_row_content);
            }
        }

        if (self.cy == self.screenrows - 1) {
            self.rowoff += 1;
        } else {
            self.cy += 1;
        }
        self.cx = 0;
        self.coloff = 0;
    }

    pub fn delChar(self: *Editor) void {
        const filerow = self.rowoff + self.cy;
        const filecol = self.coloff + self.cx;
        const row = if (filerow >= self.numrows) null else &self.row[filerow];

        if (row == null or (filecol == 0 and filerow == 0)) return;

        if (filecol == 0) {
            if (filerow == 0 or filerow >= self.numrows) return;

            const prev_row = &self.row[filerow - 1];
            const filecol_previous = prev_row.size;
            self.delRow(filerow);

            if (self.cy == 0) {
                self.rowoff -= 1;
            } else {
                self.cy -= 1;
            }

            self.cx = filecol_previous;
            if (self.cx >= self.screencols) {
                const shift = self.cx - self.screencols + 1;
                self.cx -= shift;
                self.coloff += shift;
            }

            if (filerow > 0) {
                self.updateRow(&self.row[filerow - 1]);
            }
        } else {
            self.rowDelChar(row.?, filecol - 1);
            if (self.cx == 0 and self.coloff > 0) {
                self.coloff -= 1;
            } else {
                self.cx -= 1;
            }
            self.updateRow(row.?);
        }

        self.dirty = true;
    }

    pub fn open(self: *Editor, filename: []const u8) !void {
        for (self.row) |*row| {
            row.deinit(self.allocator);
        }
        self.allocator.free(self.row);
        self.row = &[_]EditorRow{};
        self.numrows = 0;

        self.dirty = false;
        self.allocator.free(self.filename orelse "");
        self.filename = try self.allocator.dupe(u8, filename);

        const cwd = std.fs.cwd();
        var file = cwd.openFile(filename, .{ .mode = .read_only }) catch |err| {
            if (err == error.FileNotFound) {
                _ = try cwd.createFile(filename, .{});
                self.dirty = false;
                return;
            }
            return err;
        };
        defer file.close();

        var buf_reader = std.io.bufferedReader(file.reader());
        var reader = buf_reader.reader();

        var line_buf = std.ArrayList(u8).init(self.allocator);
        defer line_buf.deinit();

        while (true) {
            reader.streamUntilDelimiter(line_buf.writer(), '\n', null) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };

            if (line_buf.items.len > 0 and line_buf.items[line_buf.items.len - 1] == '\r') {
                _ = line_buf.pop();
            }

            try self.insertRow(self.numrows, line_buf.items);
            line_buf.clearRetainingCapacity();
        }

        self.dirty = false;
    }

    pub fn save(self: *Editor) !void {
        if (self.filename == null) return error.NoFilename;

        const buf = try self.rowsToString(self.allocator);
        defer self.allocator.free(buf);

        const cwd = std.fs.cwd();
        var file = try cwd.createFile(self.filename.?, .{
            .read = true,
            .truncate = true,
        });
        defer file.close();

        try file.writeAll(buf);
        self.dirty = false;
        self.setStatusMessage("Wrote {d} bytes to disk", .{buf.len});
    }

    pub fn find(self: *Editor) !void {
        const saved_cx = self.cx;
        const saved_cy = self.cy;
        const saved_coloff = self.coloff;
        const saved_rowoff = self.rowoff;

        var query = std.ArrayList(u8).init(self.allocator);
        defer query.deinit();
        var last_match: ?isize = null;
        var find_next: i8 = 0;
        var saved_hl_line: ?usize = null;
        var saved_hl: ?[]u8 = null;
        defer if (saved_hl) |hl| self.allocator.free(hl);

        const restore_hl = struct {
            fn restore(editor: *Editor, line: ?usize, hl: ?[]u8) void {
                if (line) |l| if (hl) |h| {
                    @memcpy(editor.row[l].hl, h);
                    editor.allocator.free(h);
                };
            }
        }.restore;

        while (true) {
            self.setStatusMessage("Find: {s} (Use Esc/Arrows/Enter)", .{query.items});
            try self.refreshScreen();

            const key = try self.readKey();
            switch (key) {
                DEL_KEY, CTRL_H, BACKSPACE => {
                    if (query.items.len > 0) {
                        _ = query.pop();
                        last_match = null;
                    }
                },
                ESC, ENTER => {
                    if (key == ESC) {
                        self.cx = saved_cx;
                        self.cy = saved_cy;
                        self.coloff = saved_coloff;
                        self.rowoff = saved_rowoff;
                    }
                    restore_hl(self, saved_hl_line, saved_hl);
                    self.setStatusMessage("", .{});
                    return;
                },
                ARROW_RIGHT, ARROW_DOWN => find_next = 1,
                ARROW_LEFT, ARROW_UP => find_next = -1,
                else => {
                    if (std.ascii.isPrint(@intCast(key))) {
                        try query.append(@intCast(key));
                        last_match = null;
                    }
                },
            }

            if (last_match == null) find_next = 1;
            if (find_next != 0) {
                var current: isize = if (last_match) |lm| lm else -1;

                for (0..self.numrows) |_| {
                    current += find_next;
                    if (current == -1) current = @intCast(self.numrows - 1) else if (current == @as(isize, @intCast(self.numrows))) current = 0;

                    const row = &self.row[@intCast(current)];
                    if (std.mem.indexOf(u8, row.render, query.items)) |match_offset| {
                        last_match = current;

                        restore_hl(self, saved_hl_line, saved_hl);
                        saved_hl_line = @intCast(current);
                        saved_hl = try self.allocator.dupe(u8, row.hl);
                        @memset(row.hl[match_offset .. match_offset + query.items.len], HL_MATCH);

                        self.cy = 0;
                        self.cx = match_offset;
                        self.rowoff = @intCast(current);
                        self.coloff = 0;

                        if (self.cx > self.screencols) {
                            const diff = self.cx - self.screencols;
                            self.cx -= diff;
                            self.coloff += diff;
                        }
                        break;
                    }
                }
            }
        }
    }

    fn drawStatusBar(self: *Editor, ab: *AppendBuffer) !void {
        try ab.append("\x1b[7m");
        var status: [80]u8 = undefined;
        var rstatus: [80]u8 = undefined;

        const len = std.fmt.bufPrint(&status, "{s} - {d} lines {s}", .{
            self.filename orelse "[No Name]",
            self.numrows,
            if (self.dirty) "(modified)" else "",
        }) catch unreachable;

        const rlen = std.fmt.bufPrint(&rstatus, "{d}/{d}", .{
            self.rowoff + self.cy + 1,
            self.numrows,
        }) catch unreachable;

        const status_len = len.len;
        if (status_len > self.screencols) {
            try ab.append(status[0..self.screencols]);
        } else {
            try ab.append(status[0..status_len]);
            var padding = self.screencols - status_len;
            while (padding > 0) : (padding -= 1) {
                if (padding == rlen.len) {
                    try ab.append(rstatus[0..rlen.len]);
                    break;
                } else {
                    try ab.append(" ");
                }
            }
        }
        try ab.append("\x1b[m");
        try ab.append("\r\n");
    }

    fn drawMessageBar(self: *Editor, ab: *AppendBuffer) !void {
        try ab.append("\x1b[K");
        const msglen = std.mem.sliceAsBytes(&self.statusmsg).len;
        if (msglen > 0 and
            std.time.nanoTimestamp() - self.statusmsg_time < 5 * std.time.ns_per_s)
        {
            if (msglen > self.screencols) {
                try ab.append(self.statusmsg[0..self.screencols]);
            } else {
                try ab.append(self.statusmsg[0..msglen]);
            }
        }
    }

    pub fn moveCursor(self: *Editor, key: i32) void {
        const filerow = self.rowoff + self.cy;
        const filecol = self.coloff + self.cx;
        const row = if (filerow >= self.numrows) null else &self.row[filerow];

        switch (key) {
            ARROW_LEFT => {
                if (self.cx == 0) {
                    if (self.coloff > 0) {
                        self.coloff -= 1;
                    } else if (filerow > 0) {
                        self.cy -= 1;
                        self.cx = self.row[filerow - 1].size;
                        if (self.cx > self.screencols - 1) {
                            self.coloff = self.cx - self.screencols + 1;
                            self.cx = self.screencols - 1;
                        }
                    }
                } else {
                    self.cx -= 1;
                }
            },
            ARROW_RIGHT => {
                if (row) |r| {
                    if (filecol < r.size) {
                        if (self.screencols > 0 and self.cx == self.screencols - 1) {
                            self.coloff += 1;
                        } else {
                            self.cx += 1;
                        }
                    } else if (filecol == r.size) {
                        self.cx = 0;
                        self.coloff = 0;
                        if (self.cy == self.screenrows - 1) {
                            self.rowoff += 1;
                        } else {
                            self.cy += 1;
                        }
                    }
                }
            },
            ARROW_UP => {
                if (self.cy == 0) {
                    if (self.rowoff > 0) self.rowoff -= 1;
                } else {
                    self.cy -= 1;
                }
            },
            ARROW_DOWN => {
                if (filerow < self.numrows) {
                    if (self.cy == self.screenrows - 1) {
                        self.rowoff += 1;
                    } else {
                        self.cy += 1;
                    }
                }
            },
            else => {},
        }

        const new_filerow = self.rowoff + self.cy;
        const new_filecol = self.coloff + self.cx;
        const new_row = if (new_filerow >= self.numrows) null else &self.row[new_filerow];
        const rowlen = if (new_row) |r| r.size else 0;

        if (new_filecol > rowlen) {
            self.cx -= new_filecol - rowlen;
            if (self.cx < 0) {
                self.coloff += self.cx;
                self.cx = 0;
            }
        }

        self.dirty = self.numrows > 0;
    }

    pub fn processKeypress(self: *Editor) !void {
        const key = try self.readKey();

        switch (key) {
            ENTER => try self.insertNewline(),
            CTRL_C => {},
            CTRL_Q => {
                if (self.dirty and self.quit_times > 0) {
                    self.setStatusMessage("WARNING! File has unsaved changes. Press Ctrl-Q {d} more times to quit.", .{self.quit_times});
                    self.quit_times -= 1;
                    return;
                }
                try self.refreshScreen();
                std.process.exit(0);
            },
            CTRL_S => _ = try self.save(),
            CTRL_F => try self.find(),
            BACKSPACE, CTRL_H, DEL_KEY => {
                self.quit_times = 2;
                if (key == DEL_KEY) self.moveCursor(ARROW_RIGHT);
                self.delChar();
            },
            PAGE_UP, PAGE_DOWN => {
                if (key == PAGE_UP and self.cy != 0) {
                    self.cy = 0;
                } else if (key == PAGE_DOWN and self.cy != self.screenrows - 1) {
                    self.cy = self.screenrows - 1;
                }

                var times = self.screenrows;
                while (times > 0) : (times -= 1) {
                    self.moveCursor(if (key == PAGE_UP) ARROW_UP else ARROW_DOWN);
                }
            },
            ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT => self.moveCursor(key),
            CTRL_L => {},
            ESC => {},
            else => if (key >= 0) {
                self.quit_times = 2;
                try self.insertChar(@intCast(key));
            },
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var editor = Editor{ .allocator = allocator };
    defer editor.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        std.debug.print("Usage: enkel <filename>\n", .{});
        std.process.exit(1);
    }

    try editor.enableRawMode();
    try editor.getWindowSize();
    try editor.selectSyntaxHighlight(args[1]);
    try editor.open(args[1]);

    editor.setStatusMessage("Enkel Help: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find", .{});

    while (true) {
        try editor.refreshScreen();
        try editor.processKeypress();
    }
}
