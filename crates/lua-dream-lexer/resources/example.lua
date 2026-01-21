#!/usr/local/bin/lua
-- ^ Shebang: Der Lexer sollte die erste Zeile ignorieren, wenn sie mit #! beginnt.

-- This file was created by my good friend gemini
-- A simple line comment
--[[
    A block comment
    spanning multiple lines
]]

-- 1. Variablen mit Attributen (Lua 5.4 Update)
local user_name <const> = "Gemini" 
local pi_value = 3.1415e0     -- Wissenschaftliche Notation hinzugefügt
local hex_pi = 0x1.91eb851eb851fp+1 -- Hexadezimaler Float (sehr komplex!)
local is_active = true
local nothing = nil

-- Arithmetic and Operators
local result = (10 + 5) * 2 / 4 ^ 2 % 3
local logic = 10 == 20 or 5 ~= 3 and not false

-- String Concatenation and Length
local greeting = "Hello " .. 'World'
local size = #"Length Operator"

-- Tables (The heart of Lua)
local config = {
  key = "value",
  [1] = 42; 
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

-- Loops
local count = 0
while count < 5 do
  count = count + 1
end

for i = 1, 10, 1 do
  if i == 5 then break end
end

repeat
  count = count - 1
until count <= 0 

-- Labels and Goto
::start_over::
local input = "retry"
if input == "retry" then
    local x <close> = some_resource() -- Ein <close> Attribut Test
    goto start_over 
end

-- Long Strings (Der "Final Boss" mit verschachtelten Ebenen)
local multi_line_text = [==[
    This is a string that can
    contain "quotes", symbols like =
    and even double brackets [[ ]]
    without ending prematurely.
]==]

-- Special Operators
local args = ...
local output = config:get_data() 

-- Bitwise Operators & Floor Division
local bitwise = 5 & 3 | 2 ~ 1 << 1 >> 2 // 1

-- End of file
