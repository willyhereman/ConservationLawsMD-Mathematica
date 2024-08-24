(* data file d_kdv.m *)
(* Menu item 1-1 *)

(* Last Updated:  4 September, 2008, 9:29 by DP at CSM *)

(***   KORTEWEG-DE VRIES (KdV) EQUATION   ***)
(***   u_t + u u_x + u_xxx = 0            ***)

eq[1] = D[u[1][x,t],t] + u[1][x,t]*D[u[1][x,t],x] + D[u[1][x,t],{x,3}];

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x};
nameINPUT = "Korteweg-de Vries (KdV) Equation";
noteINPUT = Null; (* The user can put any additional information about the *)
                  (* PDE here.  Information must be in quotes, "---".      *)

(**** The following input may not apply to your PDE.  If there are no    ****)
(**** parameters, an empty list, {}, must be given as input for each     ****)
(**** item.                                                              ****)
parametersINPUT = {};
weightedParametersINPUT = {};

(**** The user can choose to provide this information. If the user is not****)
(**** providing any information, an empty list or Null, as shown here,   ****)
(**** must be given as input.  Necessary infomation will be prompted as  ****)
(**** the program runs.  If the user wishes to provide information,      ****)
(**** examples are shown below.                                          ****)
userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(**** BELOW THIS POINT, SEVERAL EXAMPLES ARE GIVEN TO SHOW HOW THE DATA  ****)
(**** FILE CAN BE CHANGED TO SUIT THE USER'S NEEDS.                      ****)

(****************************************************************************)
(**** User can supply the rank for the density                           ****)

(* rankRhoINPUT = 6; *)


(****************************************************************************)
(**** The user can supply several ranks for calculating densities.  If   ****)
(**** rankRhoINPUT is set in the data file, the user should also set     ****)
(**** explicitIndependentVariablesInDensitiesINPUT to 0, 1, 2, etc. so   ****)
(**** the program will run without prompts.                              ****)

(* rankRhoINPUT = {2,4,5,6,7,8,10}; *)
(* rankRhoINPUT = Range[1,22];  *)
(* explicitIndependentVariablesInDensitiesINPUT = 1; *)


(****************************************************************************)
(**** The user can designate the highest degree for any explicit         ****)
(**** independent variables allowed in density terms.  If densities      ****)
(**** without explicit independent variables are desired this setting    ****)
(**** must be 0.                                                         ****)

(* explicitIndependentVariablesInDensitiesINPUT = 2; *)
(* will develop expressions with explicit independent *)
(* variables up to degree 2. *)


(****************************************************************************)
(**** The user can give the weights of u[1] and partial t.  The program  ****)
(**** will check the weights against the scaling symmetry of the PDE.    ****)

(* userWeightRulesINPUT = {weight[u[1]] -> 1, weight[d/dt] -> 3/2}; *)

(****************************************************************************)
(**** If the user wishes to test a density, the user can supply the form ****)
(**** of the density.  This can be done as a list of terms or as an      ****)
(**** expression. Three possibilities are shown. The form of the density ****)
(**** must be in the same variables as eq[1][x,t].                       ****)

(* For rank 6: Density terms can be given in a list. *)
(*
formRhoINPUT = {u[1][x,t]^3, D[u[1][x,t],x]^2};
*)

(* NOTE: Other examples shown here are for test purposes only. *)
(*
formRhoINPUT = {D[u[1][x,t],{x,4}]^2, c[1]*u[1][x,t],
               f[4]*u[1][x,t]*D[u[1][x,t],{x,3}],
               ee*D[u[1][x,t],x]^4};
*)

(* For rank 8: Density terms can be given with unknown coefficients, c[i]. *)
(*
formRhoINPUT = c[1]*u[1][x,t]^4+c[2]*u[1][x,t]*D[u[1][x,t],x]^2+
               c[3]*D[u[1][x,t],{x,2}]^2+
               c[4]*D[u[1][x,t],x]*D[u[1][x,t],{x,3}];
*)

(* REFERENCE:                                                           *)
(* R. M. Miura, Korteweg-de Vries Equation and Generalizations I:       *)
(* Remarkable Explicit Nonlinear Transformations,                       *)
(* J. Math. Phys. V. 9, (1967), pp. 1202-1204.                          *)

(* d_kdv.m *)
(* end of file *)
