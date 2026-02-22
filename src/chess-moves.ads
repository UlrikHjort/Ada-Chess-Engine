-- ***************************************************************************
--                      Chess - Move Generation Specification
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
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
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Chess.Types; use Chess.Types;
with Ada.Containers.Vectors;

package Chess.Moves is

   package Move_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Move_Type);

   subtype Move_List is Move_Vectors.Vector;

   function Generate_Pseudo_Legal_Moves (State  : Game_State;
                                         Square : Square_Type) 
                                        return Move_List;

   function Generate_All_Pseudo_Legal_Moves (State : Game_State;
                                             Color : Color_Type) 
                                            return Move_List;

   function Is_Square_Attacked (State    : Game_State;
                               Square   : Square_Type;
                               By_Color : Color_Type) return Boolean;

   function Is_In_Check (State : Game_State;
                        Color : Color_Type) return Boolean;

   function Is_Legal_Move (State : Game_State;
                          Move  : Move_Type) return Boolean;

   function Generate_Legal_Moves (State  : Game_State;
                                 Square : Square_Type) 
                                return Move_List;

   function Generate_All_Legal_Moves (State : Game_State;
                                     Color : Color_Type) 
                                    return Move_List;

   function Apply_Move (State : Game_State;
                       Move  : Move_Type) return Game_State;

   function Is_Checkmate (State : Game_State;
                         Color : Color_Type) return Boolean;

   function Is_Stalemate (State : Game_State;
                         Color : Color_Type) return Boolean;

end Chess.Moves;
