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
local byte = string.byte
local open = io.open
local sub = string.sub

-- Modules --
local operators = require("bitwise_ops.operators")

-- Exports --
local M = {}

--
if operators.HasBitLib() then
	-- Fanciness
end

--
local function DefByteFunc () end

--- DOCME
function M.BitReader (stream, pos, on_byte, want_reader_op)
	on_byte = on_byte or DefByteFunc

	local bits_read, bit, cur_byte, op_func = 0

	if want_reader_op then
		function op_func (op, arg)
			if op == "get_pos" or op == "get_pos_rounded_up" then
				if op == "get_pos_rounded_up" and bits_read ~= 0 then
					return pos + 1, 0
				else
					return pos, bits_read
				end
			elseif op == "round_up" and bits_read ~= 0 then
				pos, bits_read = pos + 1, 0
			elseif op == "get_bytes" then
				local from = pos

				pos = pos + arg

				return byte(stream, from, pos - 1)
			elseif op == "peek_bytes" then
				return byte(stream, pos, pos + arg - 1)
			end
		end
	end

	return function(acc, n)
		--
		while n + bits_read >= 8 do
			if bits_read == 0 then
				cur_byte = byte(stream, pos)
				pos = on_byte(cur_byte, stream, pos) or pos
			end

			local left = 8 - bits_read

			acc, n, bits_read, pos = 2^left * acc + cur_byte, n - left, 0, pos + 1
		end

		--
		for _ = 1, n do
			acc = 2 * acc

			if bits_read > 0 then
				if bits_read < 7 then
					bit, bits_read = .5 * bit, bits_read + 1

					if cur_byte >= bit then
						cur_byte, acc = cur_byte - bit, acc + 1
					end
				else
					acc, bits_read, pos = acc + cur_byte, 0, pos + 1
				end
			else
				cur_byte, bit, bits_read = byte(stream, pos), 0x80, 1
				pos = on_byte(cur_byte, stream, pos) or pos

				-- ^^^ TODO: Should be able to handle e.g. stuff bytes in JPEG
				if cur_byte >= 0x80 then
					cur_byte, acc = cur_byte - 0x80, acc + 1
				end
			end
		end

		return acc
	end, op_func
end

-- ^^^ TODO: Might benefit from special version if bit ops are present

--
local DefRowFunc = DefByteFunc

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
function M.Sub (stream, pos, n)
	return sub(stream, pos, pos + n - 1)
end

-- Export the module.
return M