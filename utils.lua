--- Some utilities for image operations.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local assert = assert
local byte = string.byte
local open = io.open
local sub = string.sub

-- Modules --
local operators = require("bitwise_ops.operators")

-- Imports --
local rshift = operators.rshift

-- Exports --
local M = {}

--[[
From the original (in zlib), for GetBits() and GetCode():

/*
 * Extracted from pdf.js
 * https://github.com/andreasgal/pdf.js
 *
 * Copyright (c) 2011 Mozilla Foundation
 *
 * Contributors: Andreas Gal <gal@mozilla.com>
 *               Chris G Jones <cjones@mozilla.com>
 *               Shaon Barman <shaon.barman@gmail.com>
 *               Vivien Nicolas <21@vingtetun.org>
 *               Justin D'Arcangelo <justindarc@gmail.com>
 *               Yury Delendik
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
]]

--
local function AuxGet (BS, n, buf, size)
	local bytes, pos, shift = BS.m_bytes, BS.m_bytes_pos, 2^size

	repeat
		buf = buf + byte(bytes, pos) * shift
		size, pos, shift = size + 8, pos + 1, shift * 256
	until shift >= n

	BS.m_bytes_pos = pos

	return buf, size
end

--- DOCME
function M.GetBits (byte_stream, bits)
	local buf, size, bsize = byte_stream.m_code_buf, byte_stream.m_code_size, 2^bits

	if size < bits then
		buf, size = AuxGet(byte_stream, bsize, buf, size)
	end

	local bval = buf % bsize

	byte_stream.m_code_buf = (buf - bval) / bsize
	byte_stream.m_code_size = size - bits

	return bval
end

--- DOCME
function M.GetCode (byte_stream, codes)
	local buf, size, csize = byte_stream.m_code_buf, byte_stream.m_code_size, codes.size

	if size < codes.max_len then
		buf, size = AuxGet(byte_stream, csize, buf, size)
	end

	local code = codes[buf % csize + 1]
	local cval = code % 2^16
	local clen = (code - cval) * 2^-16

	assert(size ~= 0 and size >= clen and clen ~= 0, "Bad encoding in byte stream")

	byte_stream.m_code_buf = rshift(buf, clen)
	byte_stream.m_code_size = size - clen

	return cval
end

--
local function DefRowFunc () end

--- DOCME
function M.ForEach (pixels, w, h, func, on_row, arg)
	on_row = on_row or DefRowFunc

	local i = 1

	for y = 1, h do
		for x = 1, w do
			func(x, y, pixels[i], pixels[i + 1], pixels[i + 2], pixels[i + 3], i, arg)

			i = i + 4
		end

		on_row(y, arg)
	end
end

--- DOCME
function M.ForEachInColumn (pixels, w, h, func, col, arg)
	local i, stride = (col - 1) * 4 + 1, w * 4

	for y = 1, h do
		func(col, y, pixels[i], pixels[i + 1], pixels[i + 2], pixels[i + 3], i, arg)

		i = i + stride
	end
end

--- DOCME
function M.ForEachInRow (pixels, w, func, row, arg)
	local i = (row - 1) * w * 4 + 1

	for x = 1, w do
		func(x, row, pixels[i], pixels[i + 1], pixels[i + 2], pixels[i + 3], i, arg)

		i = i + 4
	end
end

--
local function AuxSaveByteStream (from, into)
	into.m_bytes_pos = from.m_bytes_pos
	into.m_code_buf = from.m_code_buf
	into.m_code_size = from.m_code_size
end

--- DOCME
function M.LoadByteStream (byte_stream, from)
	AuxSaveByteStream(from, byte_stream)
end

--
local function GetContents (name, read)
	local image, contents = open(name, "rb")

	if image then
		contents = image:read(read)

		image:close()
	end

	return contents
end

--- DOCME
function M.ReadHeader (name, nread, read, get_data)
	local str = GetContents(name, nread)

	if str then
		return read(str, get_data)
	else
		return false
	end
end

--- DOCME
function M.Load (name, load, yfunc)
	local contents = GetContents(name, "*a")

	return contents and load(contents, yfunc)
end

--- DOCMEMORE
-- Reads out four bytes as an integer
function M.ReadU16 (stream, pos)
	local a, b = byte(stream, pos, pos + 1)

	return a * 2^8 + b
end

--- DOCMEMORE
-- Reads out four bytes as an integer
function M.ReadU32 (stream, pos)
	local a, b, c, d = byte(stream, pos, pos + 3)

	return a * 2^24 + b * 2^16 + c * 2^8 + d
end

--- DOCME
function M.SaveByteStream (byte_stream, into)
	AuxSaveByteStream(byte_stream, into)
end

--- DOCME
function M.GetBits (byte_stream, bits)
	local buf, size, bsize = byte_stream.m_code_buf, byte_stream.m_code_size, 2^bits



end

--- DOCME
function M.Sub (stream, pos, n)
	return sub(stream, pos, pos + n - 1)
end

-- Export the module.
return M