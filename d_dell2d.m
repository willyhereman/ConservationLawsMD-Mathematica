(* d_dell2d.m *)
(* Menu item 2-15 *)

(* Last Updated:  13 March, 2009, 10:00 by DP at CSM *)

(***  SHALLOW WATER EQUATIONS  ***)
(* 2D system for fluid dynamics by Paul Dellar                          *)

(* Assignment of the variables:                                         *)
(* u[1] = u     (first component velocity),                             *)
(* u[2] = v     (second component velocity),                            *)
(* u[3] = theta                                                         *)
(* u[4] = h                                                             *)

eq[1] = D[u[1][x,y,t],t] + u[1][x,y,t]*D[u[1][x,y,t],x] +
         u[2][x,y,t]*D[u[1][x,y,t],y] - 2*omega*u[2][x,y,t] +
         u[3][x,y,t]*D[u[4][x,y,t],x] + (1/2)*u[4][x,y,t]*D[u[3][x,y,t],x];

eq[2] = D[u[2][x,y,t],t] + u[1][x,y,t]*D[u[2][x,y,t],x] +
        u[2][x,y,t]*D[u[2][x,y,t],y] + 2*omega*u[1][x,y,t] +
        u[3][x,y,t]*D[u[4][x,y,t],y] + (1/2)*u[4][x,y,t]*D[u[3][x,y,t],y];

eq[3] = D[u[3][x,y,t],t] + u[1][x,y,t]*D[u[3][x,y,t],x] +
        u[2][x,y,t]*D[u[3][x,y,t],y];

eq[4] = D[u[4][x,y,t],t]+ u[1][x,y,t]*D[u[4][x,y,t],x] +
        u[2][x,y,t]*D[u[4][x,y,t],y] + u[4][x,y,t]*D[u[1][x,y,t],x] +
        u[4][x,y,t]*D[u[2][x,y,t],y];

diffFunctionListINPUT = Table[eq[i],{i,1,4}];
numDependentVariablesINPUT = 4;
independentVariableListINPUT = {x,y};
nameINPUT = "Fluid dynamics: Shallow Water Equations";

parametersINPUT = {};
weightedParametersINPUT = {omega};

userWeightRulesINPUT = {};

(* userWeightRulesINPUT =
       weight[omega]->3, weight[u[4]]->2, weight[d/dx]->1
*)

(* userWeightRulesINPUT = {weight[d/dt] -> 2, weight[u[1]] -> 1,
    weight[u[2]] -> 1, weight[u[3]] -> 1, weight[d/dy] -> 1,
    weight[omega] -> 2, weight[u[4]] -> 1};
*)

rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRho = {};

(* Conservation of potential vorticity does not hold for this case.     *)
(*
formRhoINPUT = (1/u[4][x,y,t])*(2 * omega -D[u[1][x,y,t],y] + D[u[2][x,y,t],x]);
*)

(* Generalized densities *)
(*
formRhoINPUT = {u[3][x,y,t]^n*u[4][x,y,t] + omega*u[3][x,y,t]^m +
u[3][x,y,t]^m*D[u[1][x,y,t],y] + u[3][x,y,t]^m*D[u[2][x,y,t],x]};
*)

(*
formRhoINPUT = {ff[u[3][x,y,t]]*u[4][x,y,t] + gg[u[3][x,y,t]]*omega+
gg[u[3][x,y,t]]*D[u[1][x,y,t],y] + gg[u[3][x,y,t]]*D[u[2][x,y,t],x]};
*)

(* REFERENCE:                                                           *)
(* P. Dellar, Common Hamiltonian structure in the shallow water         *)
(* equations with horizontal temperature gradients and magnetic fields. *)
(* Physics of Fluids, V. 15 (2003), pp. 292-297.                        *)

(* d_dell2d.m *)
(* end of file *)
