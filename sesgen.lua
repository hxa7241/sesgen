#!/usr/bin/env lua
--------------------------------------------------------------------------------
--                                                                            --
--  Software Engineering Symbol Generator (Lua 5.1 / 5.2)                     --
--  Harrison Ainsworth / HXA7241 : 2011, 2013                                 --
--                                                                            --
--  http://www.hxa.name/articles/content/software-engineering-symbol_hxa7241  --
--  _2009.html                                                                --
--                                                                            --
--  License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/         --
--                                                                            --
--------------------------------------------------------------------------------




-- Implementation description.
--
-- Data structure: a sparse 2D array, as a hash-table keyed by coords -- this
-- can hold the cells and allow then to take any form.
--
-- Algorithm: start with one cell, grow 7 more cells, then transcribe the cells
-- into a string. Grow by finding all unoccupied positions adjacent to occupied
-- cells and choosing one.




-- data structure primitive operations -----------------------------------------

function init( x, y )

   return { [coordToKey(x, y)] = makeValue() }

end


function makeValue()

   -- good enough randomness for this purpose
   return math.random( 0, 1 )

end


function coordToKey( x, y )

   -- (carefully avoid negative 0)
   return "" .. (x == 0 and 0 or x) .. " " .. y

end


function keyToCoord( key )

   return string.match( key, "(%S+) (%S+)" )

end




-- algorithmic parts -----------------------------------------------------------

-- undefined -> cells
--
function create()

   -- cells -> undefined (cells mutator)
   --
   function grow( cells )

      -- make set of potential cells valid to grow into
      local potentials = {}
      local count      = 0
      -- step through all cells
      for key, _ in pairs(cells) do
         local x, y = keyToCoord( key )

         -- look left, right, below, above
         for a = 0, 1 do
            for b = -1, 1, 2 do
               local adjacent = coordToKey( x + (a * b), y + ((1-a) * b) )
               -- include if not already occupied in cells
               if not (cells[adjacent] or potentials[adjacent]) then
                  potentials[ adjacent ] = true
                  count = count + 1
               end
            end
         end

      end

      -- choose a potential cell
      local newKey = ""
      do
         local index = math.random( count )
         -- step through until chosen index
         for key, _ in pairs(potentials) do
            if 1 == index then
               newKey = key
               break
            end
            index = index - 1
         end
      end

      -- make actual cell
      cells[ newKey ] = makeValue()

   end

   -- start with one, grow 7 more
   local cells = init( 0, 0 )
   for i = 1, 7 do
      grow( cells )
   end

   return cells

end


-- cells -> string
--
function toString( cells )

   -- get cells rectangular bound
   local min = { x=  math.huge, y=  math.huge }
   local max = { x= -math.huge, y= -math.huge }
   for key, _ in pairs(cells) do
      local x, y = keyToCoord( key )
      min.x, min.y = math.min( min.x, x ), math.min( min.y, y )
      max.x, max.y = math.max( max.x, x ), math.max( max.y, y )
   end

   -- transcribe cells into a block of chars
   local str = ""
   for y = min.y, max.y do
      for x = min.x, max.x do
         local cell = cells[ coordToKey(x, y) ]
         str = str .. (cell and cell or " ")
      end
      str = str .. "\n"
   end

   return "\n" .. str

end




-- execution control -----------------------------------------------------------

-- check if help message needed
if #arg > 0 then

   -- print help message
   print("\n" ..
      "  Software Engineering Symbol Generator (Lua 5.1 / 5.2)\n" ..
      "  Harrison Ainsworth / HXA7241 : 2013-01-26\n" ..
      "  http://www.hxa.name/\n" ..
      "\n" ..
      "Run with no arguments to generate an instance of the 'software " ..
      "engineering\n" .. 
      "symbol' -- eight bits in a four-connected structure.\n" ..
      "See: http://www.hxa.name/articles/content/software-engineering-" ..
      "symbol_hxa7241_2009.html\n")

-- execute
else

   -- (wholly) different every run
   -- (note: Lua table elements are randomly ordered -- this is a second
   -- source of randomness in the program, so setting the same seed will not
   -- produce the same results.)
   math.randomseed( math.floor(os.time()) )

   local cells = create()
   print( toString( cells ) )

end
