#!/usr/bin/env ocaml
(*------------------------------------------------------------------------------

   Software Engineering Symbol Generator (OCaml 4.00)
   Harrison Ainsworth / HXA7241 : 2013

   http://www.hxa.name/articles/content/software-engineering-symbol_hxa7241
   _2009.html

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




module List =
struct
   include List

   let rec unfold ?(list = []) fn size = if size <= 0 then
      list else unfold ~list:((fn (size - 1)) :: list) fn (size - 1)
end ;;


module CoordSet =
struct
   include Set.Make(
      struct
         let compare = Pervasives.compare
         type t = int * int
      end
   )

   let rec ofList ?(set = empty) list = if list = [] then
      set else ofList ~set:(add (List.hd list) set) (List.tl list)
end ;;


let rec loop fn i iend product =
   if i < iend then loop fn (i + 1) iend (fn i product) else product ;;




(* entry point -------------------------------------------------------------- *)

(* check if help message needed *)
if (Array.length Sys.argv) > 1 then

   print_endline "\n  \
        Software Engineering Symbol Generator (OCaml 4.00)\n  \
        Harrison Ainsworth / HXA7241 : 2013-01-26\n  \
        http://www.hxa.name/\n\
      \n\
      Run with no arguments to generate an instance of the 'software \
      engineering\n\
      symbol' -- eight bits in a four-connected structure.\n\
      See: http://www.hxa.name/articles/content/software-engineering-symbol_\
      hxa7241_2009.html\n"


(* execute *)
else

   let module C = CoordSet in
   let module L = List in


   (* make cells from randomness *)
   let create seed : (C.elt * string) list =

      (* grow incrementally.
         (precondition: cells empty, potentials singleton) *)
      let rec grow cells potentials i : C.t =
         if i > 0 then
            (* choose new cell *)
            let (x, y) = L.nth (C.elements potentials)
               (Random.int (C.cardinal potentials)) in
            (* update *)
            let cells = C.union cells (C.singleton (x, y)) in
            let potentials =
               let adjacents = C.ofList [(x+1,y); (x-1,y); (x,y+1); (x,y-1)] in
               C.diff (C.union potentials adjacents) cells in
            grow cells potentials (i - 1)
         else
            cells in

      (* associate a bit with each *)
      let color cells : (C.elt * string) list = L.combine (C.elements cells)
         (L.unfold (fun _ -> string_of_int(Random.int 2)) (C.cardinal cells)) in

      let () = Random.set_state seed in
      color (grow C.empty (C.singleton (0,0)) 8) in


   (* convert cells to string *)
   let toString cellBits =

      (* get rectangular bound of cells *)
      let xlo, ylo, xhi, yhi =
         if cellBits <> [] then
            let coords = fst (L.split cellBits) in
            let x0, y0 = L.hd coords in
            let minMax (xlo, ylo, xhi, yhi) (x,y) =
               (min xlo x, min ylo y, max xhi x, max yhi y) in
            L.fold_left minMax (x0, y0, x0, y0) coords
         else
            0, 0, 0, 0 in

      (* transcribe cells into block of chars *)
      let row y str =
         let item x str = str ^ (try (L.assoc (x, y) cellBits)
            with Not_found -> " ") in
         (loop item xlo (xhi +1) str) ^ "\n" in
      loop row ylo (yhi +1) "" in


   (* different every run *)
   let () = Random.self_init () in

   let cellBits = create (Random.get_state ()) in
   print_endline ("\n" ^ (toString cellBits)) ;;
