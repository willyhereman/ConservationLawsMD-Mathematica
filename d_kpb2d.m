(* d_kpb2d.m *)
(* Menu item 2-6 *)

(* Last Updated:  7 June, 2009, 13:40 by DP at CSM *)

(***  (2+1)-DIMENSIONAL KADOMTSEV-PETVIASHVILI-B (KP-B) EQUATION  ***)
(* u_xt + (1/4)*u_3xy + u_x*u_xy + (1/2)*u_xx*u_y + (1/4) d_x^(-1) u_yyy = 0 *)

(* Let v = d_x^(-1) u_y.  In Mathematica notation, v = u[2].                 *)

eq[1] = D[u[1][x,y,t],x,t] + (1/4)*D[u[1][x,y,t],{x,3},y] +
        D[u[1][x,y,t],x]*D[u[1][x,y,t],x,y] +
        (1/2)*D[u[1][x,y,t],{x,2}]*D[u[1][x,y,t],y] +
        (1/4)*D[u[2][x,y,t],{y,2}];

eq[2] = D[u[1][x,y,t],y] - D[u[2][x,y,t],x];

(* NOTE:  To put the KP-B equation in evolution form, y and t must be   *)
(* interchanged. Then an auxillary dependent variable is be introduced, *)
(* forming a system. The evolution system for the KP-B equation is      *)
(*
eq[1] = D[u[1][x,y,t],t] - D[u[2][x,y,t],x] ;

eq[2] = D[u[2][x,y,t], t] - u[3][x,y,t] ;

eq[3] = D[u[3][x,y,t], t] +
        4*D[u[1][x,y,t],x,y] + D[u[2][x,y,t],{x,3}] +
        4*D[u[1][x,y,t],x]*D[u[2][x,y,t],x] +
        2*D[u[1][x,y,t],{x,2}]*u[2][x,y,t];
*)

diffFunctionListINPUT = {eq[1],eq[2]};
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y};
nameINPUT = "the system for the (2+1)-dimensional Kadomtsev-Petvishvili-B"<>
            " Equation";
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<>
            "the variables y and t have been switched with each other in the "<>
            "equations given to the program." *)

parametersINPUT = {};
weightedParametersINPUT = {};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCES:                                                          *)
(* Paper: S. Yu, K. Toda, and T. Fukuyama, N-soliton solutions to a     *)
(* (2+1)-dimensional integrable equation,                               *)
(* J. Phys. A, V. 31 (1998), pp. 10181-10186.                           *)
(* PDE is (6) on p. 10181.                                              *)

(* Paper: Z. Lu and H. Zhang, Solition-like and period form solutions   *)
(* for high dimensional nonlinear evolution equations,                  *)
(* Chaos, Solitons and Fractals V. 17 (2003), pp. 669-673.              *)
(* PDE is (11) on p. 671.                                               *)

(* d_kpb2d.m *)
(* end of file *)
