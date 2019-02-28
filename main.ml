let _ = Random.init 42

(* 位置や速度を表すベクトル型 *)
type vector = float array

(* ベクトルのスカラー倍 *)
(* float -> vector -> vector *)
let ( *@ ) k v =
        let v' = Array.copy v in
        for i = 0 to ((Array.length v') - 1) do
                v'.(i) <- v'.(i) *. k
        done;
        v'

(* ベクトルの和 *)
(* vector -> vector -> vector *)
let ( +@ ) v w =
        let v' = Array.copy v in
        for i = 0 to ((Array.length v') - 1) do
                v'.(i) <- v'.(i) +. w.(i)
        done;
        v'

(* ベクトルの差 *)
(* vector -> vector -> vector *)
let ( -@ ) v w =
        let v' = Array.copy v in
        for i = 0 to ((Array.length v') - 1) do
                v'.(i) <- v'.(i) -. w.(i)
        done;
        v'

(* 粒子型。bestはパーソナルベスト *)
type particle = {
        position : vector;
        velocity : vector;
        best_position : vector;
        best_score : float;
}

(* ベクトルのサイズがnの粒子、すなわちサイズnの粒子を生成する *)
let partcile_make n =
        let max = 5.0 in
        let min = -5.0 in
        let f _ = (Random.float (max -. min)) +. min in
        {
                position = Array.init n f;
                velocity = Array.make n 0.0;
                best_position = Array.make n 0.0;
                best_score = neg_infinity;
        }

(* 粒子pの速度を計算する。gはグローバルベスト位置 *)
(* particle -> vector -> vector *)
let particle_velocity p g =
        let innertia = 0.9 in
        let accel_g = 0.8 in
        let accel_p = 0.8 in
        let v = p.velocity in
        let x = p.position in
        let bx = p.best_position in
        let r_g = Random.float 1.0 in
        let r_p = Random.float 1.0 in
        (innertia *@ v)
        +@ ((accel_g *. r_g) *@ (g -@ x))
        +@ ((accel_p *. r_p) *@ (bx -@ x))

(* 移動した粒子pを返す。パーソナルベストも更新する *)
(* particle -> vector -> (vector -> float) -> particle *)
let particle_move p g f =
        let v = particle_velocity p g in
        let x = p.position +@ v in
        let score = f x in
        let update = score > p.best_score in
        {
                position = x;
                velocity = v;
                best_position = if update then x else p.best_position;
                best_score = if update then score else p.best_score;
        }

(* 群型。bestはグローバルベスト *)
type swarm = {
        particles : particle array;
        best_position : vector;
        best_score : float;
}

(* サイズnの粒子がm個集まった群を生成する *)
let swarm_make m n =
        
        {
                particles = Array.init m (fun _ -> partcile_make n);
                best_position = Array.make n 0.0;
                best_score = neg_infinity;
        }

(* 移動した粒子群sを返す。グローバルベストも更新する *)
(* swarm -> (vector -> f) -> swarm *)
let swarm_move s f =
        let move_f p = particle_move p s.best_position f in
        let particles = Array.map move_f s.particles in
        let comp (x, s) (p : particle) =
                let px = p.best_position in
                let ps = p.best_score in
                if ps > s then (px, ps) else (x, s) in
        let (bp, bs) = Array.fold_left comp ([||], neg_infinity) particles in
        {
                particles = particles;
                best_position = bp;
                best_score = bs;
        }

let s = swarm_make 10 1

let rec hoge n s =
        if n <= 0 then s
        else
        let eval_f x =
                let abs x = if x < 0.0 then (-.x) else x in
                let x = x.(0) in
                -.(abs (x ** 3.0 -. 3.0*.x**2.0 +. 9.0*.x -. 8.0)) in
        let s' = swarm_move s eval_f in
        hoge (n - 1) s'

let s' = hoge 10000 s
