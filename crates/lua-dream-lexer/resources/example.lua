-- This file was created by my good friend gemini
-- A simple line comment
--[[
    A block comment
    spanning multiple lines
]]

local user_name = "Gemini" -- Identifier, Keyword, Assignment, String
local pi_value = 3.1415    -- Number (Float)
local is_active = true     -- Boolean Keyword

-- Arithmetic and Operators
local result = (10 + 5) * 2 / 4 ^ 2 % 3
local logic = 10 == 20 or 5 ~= 3 and not false

-- String Concatenation and Length
local greeting = "Hello " .. 'World'
local size = #"Length Operator"

-- Tables (The heart of Lua)
local config = {
  key = "value",
  [1] = 42, -- Semicolon is a valid separator in tables!
  ["nested"] = { data = {} }
}

-- Functions and Control Structures
function Calculate_Diff(a, b)
  if a <= b then
    return b - a
  elseif a >= b then
    print("Greater or equal")
  else
    print(a .. b)
  end
end

-- Long Strings (The "Final Boss" for the Lexer)
local multi_line_text = [[
    This is a string that can
    contain "quotes" and symbols
    like = or [[ without
    ending prematurely.
]]

-- Special Operators
local args = ...                 -- Triple dot for Varargs
local output = config:get_data() -- Colon for method calls

-- Bitwise Operators (Lua 5.3+)
local bitwise = 5 & 3 | 2 ~ 1 << 1 >> 2

-- End of file (Your lexer should return an EOF token here)
