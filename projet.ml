(* Adrien GUITTARD *)

open Graphics

(* Création de variables globales initialisées dans la fonction "initialisation" et "debut" et pour simplifier l'appel de certaines
fonctions (non recommandé en général dans un programme mais plus simple pour les 2 boucles fantôme et pacman) *)

let l = ref 0;; (* Largeur du labyrinthe en nombre de cases *)
let mur = ref [|[|[|false|]|]|] (* crée une ref de type bool array array array pour la matrice "contenant" le labyrinthe *)
let taille = ref 0;; (* Taille des cases en pixel *)
let ux = ref 0;; (* largeur de la fenêtre ouverte en pixel *)
let uy = ref 0;; (* hauteur de la fenêtre ouverte en pixel *)
let h = ref 0;; (* hauteur du labyrinthe en nombre de case *)
let pacman = ref 0;; (* case du pacman *)
let fantom = ref ( !l -1);; (* case du fantôme *)


(* Dans ce projet on identifie une case du labyrinthe de 2 façons différentes, les coordonnées 
cartésiennes (x,y) ou le numéro de la case, on utilise ces fonctions pour passer de l'une à l'autre  *)
let case2xy l case =
  let x = case mod l in 
  let y = case / l in 
  x,y;;

let xy2case l (x,y) =
  y*l+x;;

module UF = struct

  let create n = [|Array.init n (fun i -> i) ;Array.make n 1|];; (* crée un tableau constitué de n groupes *)

  let rec find i array =    (* renvoie la racine du groupe de i *)
    if array.(0).(i) = i then i 
      else (let acc = find (array.(0).(i)) array in array.(0).(i) <- acc; acc);;

  let union i j array = (* fait l'union entre les deux groupes de i et j *)
    let rooti = find i array in
    let rootj = find j array in
    if rooti <> rootj then (
      if array.(1).(rooti) < array.(1).(rootj) then (
        array.(1).(rootj) <- array.(1).(rootj) + array.(1).(rooti);
        array.(0).(rooti) <- rootj)
      else (
        array.(1).(rooti) <- array.(1).(rootj) + array.(1).(rooti);
        array.(0).(rootj) <- rooti)) ;;
end

let cases_adjacentes (d,x,y) = (* renvoie un couple (i,j) séparé par le mur (d,x,y) *)
  if d = 0 then (y* !l+x,y*(!l)+x+1) else (y* !l+x,y* !l+x+ !l);;


let mur_au_hasard () = (* renvoie un triplet (d, x, y) *) 
  let n = Random.int (( !l-1) * !h + !l * (!h-1)) in
  if n < (!l-1) * !h
  then (0, n mod (!l-1), n / (!l-1))
  else let n2 = n - (!l-1) * !h in (1, n2 mod !l, n2 / !l);;

let generate_lab () =  (* renvoie une matrice contenant les murs d'un labyrinthe "parfait" mur_present.(direction).(x).(y)*)
  let mur_present = [|Array.init (!l-1) ((fun _ -> Array.init !h (fun _ -> true)));Array.init !l ((fun _ -> Array.init (!h-1) (fun _ -> true)))|] in (* initialise le tableau représentant le lab *)
  let uf =   UF.create (!l* !h) in 
  let s = ref 1 in 
  while !s <> !l* !h do 
    let d,x,y = mur_au_hasard () in 
    let i,j = cases_adjacentes (d,x,y) in
    if (UF.find i uf) = (UF.find j uf) then ignore() else
    (
      UF.union  i j uf;
      mur_present.(d).(x).(y) <- false;
      s :=!s+1;  
    )     
  done;
  mur_present;;


let trace_pourtour upleftx uplefty taille_case l h = (* trace le pourtour du labyrinthe *)
  open_graph (" "^string_of_int(upleftx)^"x"^string_of_int(uplefty));
  set_color black;
  let haut = h * taille_case in
  let droite = l *taille_case in 
  let oy = (uplefty - haut) / 2 in 
  let ox = (upleftx - droite) / 2 in 
  moveto ox oy;
  lineto (ox + droite) oy;
  lineto (ox + droite) (oy+ haut);
  lineto ox (oy + haut);
  lineto ox oy;
  moveto (ox + droite) (oy);
  set_color white;
  lineto (ox+droite) (oy+taille_case);;


let trace_mur upleftx uplefty taille_case l h (d,x,y) = (* trace le mur (d,x,y) *)
  set_color black;
  let haut = h * taille_case in
  let droite = l *taille_case in 
  let oy = (uplefty - haut) / 2 in 
  let ox = (upleftx - droite) / 2 in 
  moveto (ox+(x+1)*taille_case) (oy + haut - ((y+1)*taille_case));
  if d = 1 then (
    lineto (ox + x*taille_case) (oy + haut - ((y+1)*taille_case))
  ) else (
    lineto (ox+(x+1)*taille_case) (oy+haut-y*taille_case)
  );;



let trace_lab upleftx uplefty taille_case l h mur_present = (* trace le labyrinthe *)
  trace_pourtour upleftx uplefty taille_case l h;
  for i = 0 to Array.length(mur_present)-1 do 
    for x = 0 to Array.length(mur_present.(i))-1 do
      for y = 0 to Array.length(mur_present.(i).(x))-1 do 
        if mur_present.(i).(x).(y) then trace_mur upleftx uplefty taille_case l h (i,x,y)
      done
    done
  done;;

(* dans la fonction "initialisation", soit on entre la taille de la fenêtre et la taille 
des cases à la main (comme demandé dans le sujet), soit on fait la configuration par défaut *)
let default_setting l h = 
  let taille = min (580/h) (580/l) in 
  (600,600,taille);;


(* avant de commencer une partie on demande au joueur les informations nécessaires pour initialiser le labyrinthe *)
let initialisation ()= 
  print_string "Nombre de cases : largeur ? ";
  let l' = read_int() in
  print_string "Nombre de cases : hauteur ? ";
  let h' = read_int() in
  print_string "Réglages par defauts ? [y/n] ";
  let ux' = ref 0 in 
  let uy' = ref 0 in 
  let taille' = ref 0 in 
  if read_line() = "n" then (
    print_string "upleftx ? ";
    ux' := read_int(); 
    print_string "uplefty ? ";
    uy' := read_int();
    taille' := min ((!ux'-50)/ l') ((!uy'-50)/h')
      )
      else (let (ux1,uy1,taille1) = default_setting l' h' in (ux' := ux1; uy' := uy1; taille' := taille1));
      l :=l'; taille := !taille'; ux := !ux'; uy := !uy'; h := h';;
      

(* utilisé dans plusieurs fonctions ci-dessous, teste si un mouvement est possible 
ou non (renvoie faux si on essaye de se déplacer à travers un mur par exemple)*)
exception False;;
exception True
let moove d x y mur_present = 
  try 
    if d = 0 then (
      if mur_present.(1).(x).(y-1) then raise False else raise True
        );
    if  d = 1 then (
      if mur_present.(1).(x).(y) then raise False else raise True
    );
    if d = 2 then (
      if mur_present.(0).(x-1).(y) then raise False else raise True
    );
    if d = 3 then (
      if mur_present.(0).(x).(y) then raise False else raise True 
    );
    true 
  with 
    |False -> false 
    |True -> true
    |Invalid_argument _ -> false;;


(* actualise la représentation du pacman ou du fantôme sur la fenêtre graphique *)
let update pacman taille ux uy l h prevx prevy pacOuFant =
  let haut = h * taille in
  let droite = l *taille in 
  let oy = (uy - haut) / 2 in 
  let ox = (ux - droite) / 2 in 
  let x = pacman mod l in 
  let y = pacman / l in 
  let currentx = ox+x*taille+taille/2 in
  let currenty = oy + haut - ( y * taille) - taille / 2 in
  set_color white;
  fill_circle prevx prevy (taille/2);
  if pacOuFant = 0 then 
  set_color red else 
  set_color blue;
  fill_circle (currentx) (currenty) (taille/2);
  currentx,currenty;;

(* "moove" renvoie true si un mouvement est possible 0, 1, 2, 3 sont les directions*)
(* la fonction "check" renvoie la liste des cases autour de la case de coordonnées "case" passé en argument 
elle est seulement utilisée dans la fonction path en dessous *)
let check case l mur =
  let x,y = case2xy l case in
  let res = ref [] in 
  if moove 0 x y mur then res := (x,y-1)::!res;
  if moove 1 x y mur then res := (x,y+1)::!res;
  if moove 2 x y mur then res := (x-1,y)::!res;
  if moove 3 x y mur then res := (x+1,y)::!res;
  !res;;

(* "decomp" vérifie si la case de coordonnées "case" passé en argument est présente dans 
la liste de coordonnées de cases adj (pour case adjoite) utilisé pour vérifier si le pacman est dans une des 
cases adjacentes de "case" *)
let rec decomp (adj:int list) (case:int) (l:int) = match adj with
  |[] -> false
  |hd::tl when hd = case -> true
  |hd::tl -> false || decomp tl case l;;

(* Path : Prend en argument la matrice du labyrinthe, la position du pacman, 
la position du fantôme et la largeur du labyrinthe et renvoie la prochaine case (x,y) que 
le fantôme doit emprunter pour rejoindre le pacman *)
let path mur pac fant l =
  let rec aux mur pac casepre l fant = (* fonction auxiliaire qui renvoie true quand le fantôme doit emprunter la case "casepre" pour se rapprocher du pacman *)
    let adj = check fant l mur in (* liste de coordonnées des cases autour du fantôme *)
    let adj = (List.filter (fun i -> not (i=casepre)) (List.map (xy2case l) adj)) in match adj with (* un peu compliqué mais on retire simplement la case précedente de la liste pour éviter une boucle infinie et on passe les coordonnées de la liste en coordonnées de "case" (donc de (x,y) à case *)
      |[] -> false
      |adj when decomp adj pac l  -> true
      |adj -> List.fold_left (||) false (List.map (aux mur pac fant l) adj) in
  let adj = check fant l mur in 
  let rec aux2 liste mur pac fant l = match liste with
    |[] -> invalid_arg "Liste vide" (* seul cas possible quand le fantôme va se déplacer sur le pacman (l'exception est recupérée dans la boucle du fantôme *)
    |hd::tl -> if (aux mur pac fant l (xy2case l hd)) then xy2case l hd else aux2 tl mur pac fant l in 
  aux2 adj mur pac fant l;;


let fin arg = (* affichage de fin (gagné ou perdu) et ferme le programme après 2 secondes *)
  clear_graph ();
  set_color black;
  if arg = 0 then (
    let larg, haut = text_size "Game Over" in
    moveto ((!ux -larg)/2) ((!uy - haut)/ 2 );
    draw_string "Game over";
  ) 
  else(
    let larg,haut = text_size "Congrats, you won !" in
    moveto ((!ux -larg)/2) ((!uy - haut)/ 2 );
    draw_string "Congrats, you won !";
  );
  Unix.sleep 2;
  exit 0;;

let win = ref false  (* change en true si le pacman arrive à la fin du labyrinthe *)

let bouclepacman () = (* boucle du pacman *)
  let prevx = ref (((!ux - !l * !taille) / 2 ) + !taille / 2) in
  let prevy = ref ((!uy - !h* !taille)/2 + !taille /2) in
  let prex,prey = update !pacman !taille !ux !uy !l !h !prevx !prevy 0 in (* utilisé pour mettre à jour l'affichage du pacman *)
  prevx := prex; prevy := prey;
  while not (!win) do(
    if !pacman = !fantom then fin 0; (* teste si le fantôme "est sur" le pacman *)
    let key = read_key() in
    (* gère les déplacements en fonction de la touche pressée *)
    if key = 'q' then (
      if moove 2 (!pacman mod !l) (!pacman / !l) !mur then (
        pacman:= !pacman-1;
        let prex,prey = update !pacman !taille !ux !uy !l !h !prevx !prevy 0 in 
        prevx := prex ; prevy := prey;
      ) 
    );
    if key = 's' then(
      if moove 1 (!pacman mod !l) (!pacman / !l ) !mur then (
        pacman := !pacman+ !l;
        let prex,prey = update !pacman !taille !ux !uy !l !h !prevx !prevy 0 in 
        prevx := prex ; prevy := prey;
      )
    );
    if key = 'd' then (
      if moove 3 (!pacman mod !l) (!pacman / !l ) !mur then (
        pacman := !pacman+1;
        let prex,prey = update !pacman !taille !ux !uy !l !h !prevx !prevy 0 in 
        prevx := prex ; prevy := prey;
      )
    );
    if key = 'z' then (
      if moove 0 (!pacman mod !l) (!pacman / !l ) !mur then (
        pacman := !pacman- !l;
        let prex,prey = update !pacman !taille !ux !uy !l !h !prevx !prevy 0 in 
        prevx := prex ; prevy := prey;
      )
    );
  );
  if !pacman = !l* !h-1 then win := true ; (* si le pacman est à la fin on change le ref "win"*)
done;
fin 1;; (* Gagné *)


let bouclefantome ()  = (* boucle du fantôme *)
  let prevx = ref 0 in (* Les trois premières lignes sont utilisées pour gérer la mise à jour de l'affiche du pacman *)
  let prevy = ref 0 in 
  let prevx',prevy' = update !fantom !taille !ux !uy !l !h (((!ux - !l * !taille) / 2 ) + !taille / 2) ((!uy - !h* !taille)/2 + !taille /2) 1 in 
  prevx := prevx'; prevy := prevy';
  try
  while (true) do 
    Unix.sleep 2;
    if !win then Thread.exit (); (* Si le pacman a gagné on arrête la boucle *)
    fantom := path !mur !pacman !fantom !l; (* le fantôme se déplace sur la case qui le rapproche du pacman *)
    let previx,previy = update !fantom !taille !ux !uy !l !h !prevx !prevy 1 in (* gère l'affichage *)
    prevx := previx;
    prevy := previy;
    done
  with |invalid_arg -> fin 0;; (* dans la fonction "path" le fameux cas où le fantôme est dans une des cases adjacentes au pacman (et est sur le point de se déplacer) *)

let debut () = (* fonction appelée pour commencer une nouvelle partie *)
  open_graph " 10x10";
  set_color black;
  close_graph (); (* au cas où l'on relance une partie, et que la couleur n'est pas sur noir *)
  initialisation ();
  fantom := !l -1;
  pacman := 0;
  mur := generate_lab ();
  trace_lab !ux !uy !taille !l !h !mur;
  ignore (Thread.create bouclefantome ());
  bouclepacman ();;


debut ();;

