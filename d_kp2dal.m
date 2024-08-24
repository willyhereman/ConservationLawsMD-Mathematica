(* d_kp2dal.m *)
(* Menu item 2-7 *)

(* Last Updated:  7 June, 2009, 15:20 by DP at CSM *)

(***  An alternate form of the KADOMTSEV-PETVIASHVILI EQUATION   ***)
(*    (u_xxx  - 2*(u_x)^2  - 4u_t)_x  - 6u_xx u_y + 3u_yy = 0      *)

eq[1] = D[aa*D[u[1][x,y,t],{x,3}] - 2*D[u[1][x,y,t],x]^3 -
        4*D[u[1][x,y,t],t],x] -
        6*D[u[1][x,y,t],{x,2}]*D[u[1][x,y,t],y] +
        3*D[u[1][x,y,t],{y,2}];

(* NOTE:  To put the alternative KP equation in evolution form, y and t *)
(* must be interchanged. Then an auxillary dependent variable is be     *)
(* introduced,  forming a system. The evolution system for the          *)
(* alternative KP equation is                                           *)
(*
eq[1] = D[u[1][x,y,t],t] - u[2][x,y,t] ;

eq[2] = D[u[2][x,y,t], t] + (1/3)*(D[-4*aa*D[u[1][x,y,t],y] -
        2*D[u[1][x,y,t],x]^3 + aa*D[u[1][x,y,t],{x,3}],x] -
        6*D[u[1][x,y,t],{x,2}]*D[u[1][x,y,t],y]);
*)

diffFunctionListINPUT = {eq[1]};
numDependentVariablesINPUT = 1;
independentVariableListINPUT = {x,y};
nameINPUT = "an alternative form for the (2+1)-dimensional"<>
            " Kadomtsev-Petvishvili Equation"
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<>
            "the variables y and t have been switched with each other in the "<>
            "equations given to the program." *)

parametersINPUT = {};
weightedParametersINPUT = {aa};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)
formRhoINPUT = {};

(* REFERENCES:                                                          *)
(* W. Hereman, Symbolic Software for Lie Symmetry Analysis, The CRC     *)
(* Handbook of Lie Group Analysis of Differential Equations,            *)
(* N. H. Ibragimov, ed., V. 3 (1994)                                    *)

(* d_kp2dal.m *)
(* end of file *)
