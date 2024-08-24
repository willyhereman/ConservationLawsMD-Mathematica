(* d_kd2d.m *)
(* Menu item 2-10 *)

(* Last Updated:  16 June, 2008, 17:39 by DP at CSM *)

(* The (2+1) GARDNER EQUATION as proposed by Konopelchenko and          *)
(* Dubrovsky equation                                                   *)

eq[1] = D[u[1][x,y,t],y] - D[u[2][x,y,t],x];

eq[2] = -D[u[1][x,y,t],t] + D[u[1][x,y,t],{x,3}] +
        6*beta*u[1][x,y,t]*D[u[1][x,y,t],x] -
        (3/2)*alpha^2*u[1][x,y,t]^2*D[u[1][x,y,t],x] +
        3*D[u[2][x,y,t],y] -
        3*alpha*D[u[1][x,y,t],x]*u[2][x,y,t];

(* NOTE:  To put the Gardner equation in evolution form, y and t must   *)
(* interchanged.  The evolution system for the Gardner equation is      *)
(*
eq[1] = D[u[1][x,y,t],t] - D[u[2][x,y,t],x];

eq[2] = -D[u[2][x,y,t], t] + (1/3)*(D[u[1][x,y,t],y] -
        D[u[1][x,y,t],{x,3}] - 6*beta*u[1][x,y,t]*D[u[1][x,y,t],x] +
        (3/2)*alpha^2*u[1][x,y,t]^2*D[u[1][x,y,t],x] +
        3*alpha*D[u[1][x,y,t],x]*u[2][x,y,t]);
*)

diffFunctionListINPUT = Table[eq[i],{i,1,2}];
numDependentVariablesINPUT = 2;
independentVariableListINPUT = {x,y};
nameINPUT = "(2+1)-dimensional Gardner Equation";
(* noteINPUT = "To create evolution equations with respect to the t-variable, "<> 
             "the variables y and t have been switched with each other in "<>   
             "the equations given to the program.
*)

parametersINPUT = {alpha,beta};
weightedParametersINPUT = {beta};

userWeightRulesINPUT = {};
rankRhoINPUT = Null;
explicitIndependentVariablesInDensitiesINPUT = Null;
formRhoINPUT = {};

(* formRhoINPUT only works with the evolution system.  The conservation *)
(* laws being tested must be transformed to match the evolution system. *)

(* Densities in general forms.                                          *)
(*
formRhoINPUT = {f[y]*(alpha*u[1][x,y,t]^2 + u[2][x,y,t]) +
               D[f[y],y]*t*u[1][x,y,t]};
*)
(*
formRhoINPUT = f[y]*(alpha*u[1][x,y,t]^3 + u[1][x,y,t]*u[2][x,y,t]) +
               (t/alpha)*D[f[y],y]*(alpha*u[1][x,y,t]^2 + u[2][x,y,t]) +
               (x/alpha)*D[f[y],y]*u[1][x,y,t] +
               (t^2/alpha)*D[f[y],{y,2}]*u[1][x,y,t];
*)
(*
formRhoINPUT = (f[y] + (alpha/beta)*t*D[f[y],y])*(alpha*u[1][x,y,t]^3 +
               u[1][x,y,t]*u[2][x,y,t]) +
               (1/beta)*(x*D[f[y],y] + t^2*D[f[y],{y,2}])*
               (alpha*u[1][x,y,t]^2 + u[2][x,y,t]) +
               (1/beta)*(t*x*D[f[y],{y,2}] + t^3*D[f[y],{y,3}])*
               u[1][x,y,t];
*)

(* REFERENCE:                                                           *) 
(* B. G. Konopelchenko and V. G. Dubrovsky, Some new integrable         *)
(* nonlinear evolution equations in 2+1 dimensions                      *)
(* Phys. Lett., V. 102A, (1984), pp.15-17.                              *)

(* d_kd2d.m *)
(* end of file *)
