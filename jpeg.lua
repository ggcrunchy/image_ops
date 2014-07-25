--- A JPEG loader and associated functionality.

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
local char = string.char
local floor = math.floor
local ipairs = ipairs
local max = math.max
local min = math.min
local sub = string.sub

-- Modules --
local image_utils = require("image_ops.utils")
local operators = require("bitwise_ops.operators")

-- Imports --
local bnot = operators.bnot
local ReadU16 = image_utils.ReadU16

-- Exports --
local M = {}

-- --
local Component = { "Y", "Cb", "Cr", "I", "Q" }

--
local function Nybbles (n)
	local low = n % 16

	return (n - low) * 2^-4, low
end

--
local function GetStartOfFrame (str, pos, get_data)
	local nbits, data = byte(str, pos)
	local h = ReadU16(str, pos + 1)
	local w = ReadU16(str, pos + 3)

	if get_data then
		data, pos = { nbits = nbits }, pos + 5

		for i = 1, byte(str, pos) * 3, 3 do
			local horz, vert = Nybbles(byte(str, pos + i + 1))

			data[#data + 1] = {
				id = Component[byte(str, pos + i)],
				horz_samp_factor = horz, vert_samp_factor = vert,
				quantization_table = byte(str, pos + i + 2)
			}
		end
	end

	return w, h, data
end

--
local function ReadHeader (str, pos, get_data)
	while true do
		if byte(str, pos) ~= 0xFF then
			return false
		else
			local code = byte(str, pos + 1)

			-- Start Of Frame --
			if code == 0xC0 or code == 0xC1 then
				return true, GetStartOfFrame(str, pos + 4, get_data)

			-- End Of Image --
			elseif code == 0xD9 then
				return false
			end
		end

		pos = pos + ReadU16(str, pos + 2) + 2
	end
end

--
local function AuxInfo (str, get_data)
	if sub(str, 1, 2) == char(0xFF, 0xD8) then
		return ReadHeader(str, 3, get_data)
	end

	return false
end

--- DOCME
function M.GetInfo (name, get_data)
	return image_utils.ReadHeader(name, "*a", AuxInfo, get_data)
end

--- DOCME
M.GetInfoString = AuxInfo

--
local function OnByte (cur_byte, jpeg, pos)
	if cur_byte == 0xFF then
		local next_byte = byte(jpeg, pos + 1)

		if next_byte == 0x00 then
			return pos + 1
		elseif next_byte == 0xFF then
			-- Fills, skip...
		else
			-- Segment :(
		end
	end
end
local oc=os.clock
--
local function Decode (get_bits, ht)
	local code = 0

	for nbits = 1, #ht do
		local symbols = ht[nbits]

		code = get_bits(code, symbols.nbits)

		if code < symbols.beyond then
			return symbols[code - symbols.offset]
		end
	end
end

--
local function Extend (v, t)
	if v < 2^(t - 1) then
		v = v - bnot(0xFFFFFFFF * 2^t)
	end

	return v
end

--
local function DHT (jpeg, from)
	local ht, spos, code, prev = {}, from + 17, 0, 0

	for i = 1, 16 do
		local n = byte(jpeg, from + i)

		if n > 0 then
			local symbols = { nbits = i - prev, offset = code - 1, beyond = code + n }

			for j = 1, n do
				symbols[j], spos = byte(jpeg, spos), spos + 1
			end

			ht[#ht + 1], code, prev = symbols, code + n, i
		end

		code = 2 * code
	end

	return ht, spos
end

--
local function DQT (jpeg, from, qbyte)
	local is_16bit = qbyte > 16
	local qt = { is_16bit = is_16bit }

	if is_16bit then
		for pos = from + 1, from + 128, 2 do
			local a, b = byte(jpeg, pos, pos + 1)

			qt[#qt + 1] = a * 2^8 + b
		end
	else
		for i = 1, 64 do
			qt[i] = byte(jpeg, from + i)
		end
	end

	return qt, from + (is_16bit and 128 or 64) + 1
end

-- --
local Cos, Sqrt2_2 = {}, .25 * math.sqrt(2)

do
	for x = 0, 7 do
		local k = 2 * x + 1

		Cos[#Cos + 1] = Sqrt2_2

		for u = 1, 7 do
			Cos[#Cos + 1] = .5 * math.cos(k * u * math.pi / 16)
		end
	end
end

-- --
local Zagzig = {
    0,  1,  8, 16,  9,  2,  3, 10,
   17, 24, 32, 25, 18, 11,  4,  5,
   12, 19, 26, 33, 40, 48, 41, 34,
   27, 20, 13,  6,  7, 14, 21, 28,
   35, 42, 49, 56, 57, 50, 43, 36,
   29, 22, 15, 23, 30, 37, 44, 51,
   58, 59, 52, 45, 38, 31, 39, 46,
   53, 60, 61, 54, 47, 55, 62, 63
}

-- Make the array more amenable to 1-based indexing.
for i, n in ipairs(Zagzig) do
	Zagzig[i] = n + 1
end

-- --
local ZZ = {}

--
local function FillZeroes (k, n)
	for i = k, k + n - 1 do
		ZZ[i] = 0
	end

	return k + n
end

-- --
local Dequant = {}
local pmcu_t,pmcu_n=0,0
local entropy_t,entropy_n=0,0
local idct_t,idct_n=0,0
local kk, kn=0,0
-- --
local QU = {}

--
local function ProcessMCU (get_bits, scan_info, shift, reader_op, yfunc)
local t0=oc()
	--
	local mcus_left, preds = scan_info.left, scan_info.preds

	if mcus_left == 0 then
		reader_op("round_up")
		reader_op("get_bytes", 2)

		for i = 1, #scan_info do
			preds[i] = 0
		end

		scan_info.left = scan_info.mcus - 1
	elseif mcus_left then
		scan_info.left = mcus_left - 1
	end

	--
	for i, scan in ipairs(scan_info) do
		local dht, aht, qt, hsf, work, wpos = scan.dht, scan.aht, scan.qt, scan.hsf, scan.work, 1

		for y = 1, scan.vsf do
			for x = 1, hsf do
local tt=oc()
				--
				local s = Decode(get_bits, dht)
				local extra, r = s % 16, 0

				if extra > 0 then
					r = get_bits(0, extra)
				end

				preds[i] = preds[i] + Extend(r, s)

				--
				local k, size = 1, 64

				while k < 64 do
					local zb = Decode(get_bits, aht)

					if zb > 0 then
						if zb < 16 then
							ZZ[k], k = Extend(get_bits(0, zb), zb), k + 1
						elseif zb ~= 0xF0 then
							local nzeroes, nbits = Nybbles(zb)

							k = FillZeroes(k, nzeroes)
							ZZ[k], k = Extend(get_bits(0, nbits), nbits), k + 1
						else
							k = FillZeroes(k, 16)
						end
					else
						local kp7 = k + 7

						size = kp7 - kp7 % 8

					--	FillZeroes(zz, k, size - k)

						break
					end
				end
entropy_t,entropy_n=entropy_t+oc()-tt,entropy_n+1
				--
				Dequant[1] = preds[i] * qt[1]

				-- for j = 1, 64 * 64, 64 ?
				--		pqt * Sqrt2_2
				-- Row1.limit = 1 (or 8?)

				for j = 2, k do--size do
					local at, dq = Zagzig[j], qt[j] * ZZ[j - 1]

					Dequant[at] = dq
				end
kk,kn=kk+k,kn+1
				for j = k--[[size]] + 1, 64 do
					Dequant[Zagzig[j]] = 0
				end

				-- ^^^ Possible way to precalculate in the cos coefficients:
				-- Zigzag[j] refers to first of 8 values in Dequant (with spacing of... 64? would then require no changes...)
				-- Do `% 8`, etc. on result to figure out row, column (alternatively, step the zagzig algorithmically)
				-- If in first column (including DC) multiply by Sqrt2_2 each
				-- Else multiply by Cos[u + col - 1], Cos[u + col + 8 - 1], ...
				-- Lookups in below step would be far fewer
				-- If matrices typically sparse, ought to incur FAR fewer multiplications and lookups overall
				-- Problem: naive approach might waste a lot of effort zeroing Dequant
				-- Idea: Keep "largest column" per row, zero until previous limit
				-- Since only used in next step, can assume further elements are 0 (even across components and MCU's)
				-- ^^ Observed non-zeroes (out of 64): simple image = avg of 21.93; complex image = 39.79
local tb=oc()
				--
				local qi = 1

				for u = 1, 64, 8 do
					local a, b, c, d, e, f, g = Cos[u + 1], Cos[u + 2], Cos[u + 3], Cos[u + 4], Cos[u + 5], Cos[u + 6], Cos[u + 7]

					for j = 1, 64, 8 do
						QU[qi], qi = Dequant[j] * Sqrt2_2 +
							a * Dequant[j + 1] +
							b * Dequant[j + 2] +
							c * Dequant[j + 3] +
							d * Dequant[j + 4] +
							e * Dequant[j + 5] +
							f * Dequant[j + 6] +
							g * Dequant[j + 7], qi + 1
					end

					QU[u] = QU[u] * Sqrt2_2 + shift
				end

				--
				local up_to = wpos + 64

				for v = 1, size, 8 do
					local a, b, c, d, e, f, g = Cos[v + 1], Cos[v + 2], Cos[v + 3], Cos[v + 4], Cos[v + 5], Cos[v + 6], Cos[v + 7]
 
					for u = 1, 64, 8 do
						work[wpos], wpos = QU[u] +
							a * QU[u + 1] +
							b * QU[u + 2] +
							c * QU[u + 3] +
							d * QU[u + 4] +
							e * QU[u + 5] +
							f * QU[u + 6] +
							g * QU[u + 7], wpos + 1
					end
				end

				--
				while wpos < up_to do
					work[wpos], wpos = 0, wpos + 1
				end

idct_t,idct_n=idct_t+oc()-tb,idct_n+1
				-- Add component to block, scale, etc.
				-- PutAtPos(zz, ...)
			end

			yfunc()
		end
	end
pmcu_t,pmcu_n=pmcu_t+oc()-t0,pmcu_n+1
end

-- --
local Synth = {}

--
function Synth.YCbCr8 (data, pos, scan_info, base, run, step)
	local y_work, cb_work, cr_work = scan_info[1].work, scan_info[2].work, scan_info[3].work

	for i = base + 1, base + run do
		local y = min(max(0, y_work[i]), 255)
		local cb = min(max(0, cb_work[i]), 255)
		local cr = min(max(0, cr_work[i]), 255)
		local r = floor(cr * (2 - 2 * .299) + y) + 128
		local b = floor(cb * (2 - 2 * .114) + y) + 128
		local g = floor((y - .114 * b - .299 * r) / .587) + 128

		data[pos], data[pos + 1], data[pos + 2], data[pos + 3] = r, g, b, 1

		pos = pos + step
	end
end

--
local function SetupScan (jpeg, from, state, dhtables, ahtables, qtables, n, restart)
	local scan_info, preds, synth = { left = restart, mcus = restart }, {}, ""

	for i = 1, n do
		local comp = Component[byte(jpeg, from + 1)]

		for _, scomp in ipairs(state) do
			if comp == scomp.id then
				local di, ai = Nybbles(byte(jpeg, from + 2))

				scan_info[i], preds[i] = {
					hsf = scomp.horz_samp_factor, vsf = scomp.vert_samp_factor,
					dht = dhtables[di + 1], aht = ahtables[ai + 1],
					qt = qtables[scomp.quantization_table + 1],
					work = {}
				}, 0
				-- TODO: hreps, vreps
---[[
for j = 1, 400 do
	scan_info[i].work[j]=0
end
--]]
				break
			end
		end

		from, synth = from + 2, synth .. comp
	end

	--
	scan_info.preds = preds

	return scan_info, assert(Synth[synth .. state.nbits], "No synthesize method available")
end

-- Default yield function: no-op
local function DefYieldFunc () end

--
local function AuxLoad (jpeg, yfunc)
	local state, w, h

	assert(sub(jpeg, 1, 2) == char(0xFF, 0xD8), "Image is not a JPEG")

	yfunc = yfunc or DefYieldFunc

	local pos, total, ahtables, dhtables, qtables, pixels, restart = 3, #jpeg, {}, {}, {}

local tt=oc()
	while true do
		assert(byte(jpeg, pos) == 0xFF, "Not a segment")

		local code = byte(jpeg, pos + 1)

		if code == 0xD9 then
			break
		end

		local from, len = pos + 4, ReadU16(jpeg, pos + 2)
		local next_pos = pos + len + 2

		--
		yfunc()

		-- Start Of Frame --
		if code == 0xC0 or code == 0xC1 then
			w, h, state = GetStartOfFrame(jpeg, from, true)

			for _, comp in ipairs(state) do
				state.hmax = max(state.hmax or 0, comp.horz_samp_factor)
				state.vmax = max(state.vmax or 0, comp.vert_samp_factor)
			end

		-- Define Huffman Table --
		elseif code == 0xC4 then
			repeat
				local hbyte, ht, spos = byte(jpeg, from), DHT(jpeg, from)

				if hbyte >= 16 then
					ahtables[hbyte - 15] = ht
				else
					dhtables[hbyte + 1] = ht
				end

				from = spos
			until from == next_pos

		-- Start Of Scan --
		elseif code == 0xDA then
			pixels = pixels or {}

			--
			local n, shift = byte(jpeg, from), 2^(state.nbits - 1)
			local scan_info, synth = SetupScan(jpeg, from, state, dhtables, ahtables, qtables, n, restart)

			--
			local get_bits, reader_op = image_utils.BitReader(jpeg, from + 2 * n + 4, OnByte, true)
			local hcells, vcells = state.hmax * 8, state.vmax * 8

			-- Work out indexing...
			local ybase, xstep, ystep = 0, 4 * hcells, 4 * w

			for y1 = 1, h, vcells do
				local y2 = min(y1 + vcells - 1, h)

				for x1 = 1, w, hcells do
					ProcessMCU(get_bits, scan_info, shift, reader_op, yfunc)

					local cbase, x2 = 1, min(x1 + hcells - 1, w)
					local run = x2 - x1 + 1

					for _ = y1, y2 do
						synth(pixels, ybase, scan_info, cbase, run, xstep)

						cbase, ybase = cbase + hcells, ybase + ystep
					end
				end
			end

			next_pos = reader_op("get_pos_rounded_up")

		-- Define Quantization table --
		elseif code == 0xDB then
			repeat
				local qbyte = byte(jpeg, from)

				qtables[qbyte % 16 + 1], from = DQT(jpeg, from, qbyte)
			until from == next_pos

		-- Define Restart Interval --
		elseif code == 0xDD then
			restart = ReadU16(jpeg, from)
		end

		pos = next_pos

		assert(pos <= total, "Incomplete or corrupt JPEG file")
	end
print("TOTAL", oc()-tt)

print("MCU", pmcu_t, pmcu_t / pmcu_n)
print("   ENTROPY", entropy_t, entropy_t / entropy_n)
print("   IDCT", idct_t, idct_t / idct_n)
print("Average occupancy", kk / kn)
	--
	local JPEG = {}

	--- DOCME
	function JPEG:ForEach (func, arg)
	--	pixels = pixels or Synthesize()

		image_utils.ForEach(pixels, w, h, func, nil, arg)
	end

	--- DOCME
	function JPEG:ForEach_OnRow (func, on_row, arg)
	--	pixels = pixels or Synthesize()

		image_utils.ForEach(pixels, w, h, func, on_row, arg)
	end

	--- DOCME
	function JPEG:ForEachInColumn (func, col, arg)
	--	pixels = pixels or Synthesize()

		image_utils.ForEachInColumn(pixels, w, h, func, col, arg)
	end

	--- DOCME
	function JPEG:ForEachInRow (func, row, arg)
	--	pixels = pixels or Synthesize()

		image_utils.ForEachInRow(pixels, w, func, row, arg)
	end

	--- DOCME
	function JPEG:GetDims ()
		return w, h
	end

	--- DOCME
	function JPEG:GetPixels ()
	--	pixels = pixels or Decode()

		return pixels
	end

	--- DOCME
	function JPEG:SetYieldFunc (func)
		yfunc = func or DefYieldFunc
	end

	return JPEG
end

--- DOCME
function M.Load (name, yfunc)
	return image_utils.Load(name, AuxLoad, yfunc)
end

--- DOCME
M.LoadString = AuxLoad

-- Export the module.
return M