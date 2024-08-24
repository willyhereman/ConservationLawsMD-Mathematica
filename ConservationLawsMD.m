(* Time-stamp: 4 February, 2010, 13:40 Mountain Daylight Time *)
(* Last updated: 6 June, 2024, 10:20 AM in Houston by U. Goktas *)

(*****************************************************************************)
(*                                                                           *)
(*          *** M A T H E M A T I C A   P R O G R A M ***                    *)
(*                                                                           *)
(*      SYMBOLIC COMPUTATION of CONSERVED DENSITIES for SYSTEMS of           *)
(*                    NONLINEAR EVOLUTION EQUATIONS                          *)
(*                                                                           *)
(* Program name: conservationLawsMD                                          *)
(*                                                                           *)
(* Purpose: To compute conserved densities and their corresponding fluxes    *)
(*          along with possible compatibility conditions of nonlinear        *)
(*          evolution PDEs in (n+1)-dimensions, where n = 1, 2, or 3.        *)
(*          To compute associated fluxes and verify the conservation laws.   *)
(*          To verify the linear independence of densities.                  *)
(*                                                                           *)
(* Input to conservationLawsMD:                                              *)
(*          System of nonlinear evolution equations of any order,            *)
(*          in any degree that haver polynomial terms. The system can have   *)
(*          independent variables {x, t}, {x, y, t} or {x, y, z, t}.         *)
(*          Any parameters must be constant; they cannot occur as functions  *)
(*          of the independent variables. A typical equation (system) can be *)
(*          written in the form:                                             *)
(*                                                                           *)
(*          u[i]_t = f(u[1],...,u[N],u[1]_{nx},...,u[N]_{nx})                *)
(*                   with i = 1,...,N; and n = 1, 2, ...                     *)
(*                                                                           *)
(* Output : Density and flux of desired rank (if it exists),                 *)
(*          and compatibility conditions on parameters, if applicable.       *)
(*                                                                           *)
(* Language : Mathematica, versions 5, 6, and 7                              *)
(*                                                                           *)
(* Authors: Douglas Poole and Willy Hereman                                  *)
(*          Department of Mathematical and Computer Sciences                 *)
(*          Colorado School of Mines                                         *)
(*          Golden, CO 80401-1887, U.S.A.                                    *)
(*                                                                           *)
(* Version 1.2.1: 4 February, 2010                                            *)
(*                                                                           *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(*****************************************************************************)
(* commentinter[]: prints a welcome message                                  *)
(*****************************************************************************)
commentinter[] := Block[{},
Print["*********************************************************"];
Print["        WELCOME TO THE MATHEMATICA PROGRAM               "];
Print["     FOR THE COMPUTATION OF CONSERVATION LAWS            "];
Print["   OF NONLINEAR PARTIAL DIFFERENTIAL EQUATIONS           "];
Print["           IN MULTIPLE SPACE DIMENSIONS                  "];
Print["        by DOUGLAS POOLE and WILLY HEREMAN               "];
Print["       Version 1.2.1 released on February 4, 2010        "];
Print["                                                         "];
Print["*********************************************************"];
Print[" "];
]; (* end commentinter *)

(* ##########              Function: newMenu               ########## *)

(**********************************************************************)
(* newMenu (no arguments)                                             *)
(* Purpose: A menu to choose between the 1-D and the multi-D cases    *)
(* Authors: Maxi von Eye, Lindsay Auble, Scott Danford                *)
(* Input:   When the program is first run, this menu is printed for   *)
(*          the user to choose if they want to run 1-D or multi-D     *)
(*          cases.                                                    *)
(* Output:  The menu is loaded for 1-D or multi-D.                    *)
(* Adapted from:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whnemenu.m, Created 1 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpnemenu.m                      *)
(* Last Modified:  5 August, 2008, 16:29 by DP at CSM                 *)
(**********************************************************************)

newMenu :=
Module[{choice, makeChoice, choice2, choice3},

  (* Make sure dependent and independent variables are clear.           *)
  Clear[u, x, y, z, t];

  commentinter[];

  Print[" "];
  Print["  *** MENU INTERFACE *** "];
  Print["-------------------------------------------"];
  Print["   1) Cases in (1+1) Dimensions"];
  Print["   2) Cases in (2+1) and (3+1) Dimensions"];
  Print["  tt) Take Equation or System from a File"];
  Print["  qq) Exit the Program "];
  Print["-------------------------------------------"];

  choice = Input["ENTER YOUR CHOICE: "];

  makeChoice := Switch[choice,
      1, menu,
      2, menu3D,
      tt, Print["Make sure that you have prepared the data file for the"<>
              " system you want to test (similar to the data files we"<>
              " supplied)."];
           choice2 = Input["If your file is ready, press 1, else 2: "];
           If[choice2 === 1,
             choice3 = InputString["Enter the name of your data file: "];
             Get[choice3],
             Print["All computations are being discontinued."];
             Print["Prepare your data file first, then restart the program."];
             Abort[];
           ],
      qq, Print["All computations are being discontinued."]; Abort[],
      _,  choice = Input["Please enter a choice from the menu."];
          makeChoice
    ]; (* end Switch *)

  Check[makeChoice, Abort[]];

  (* Clear all local variables not being returned.                      *)
  Clear[choice, makeChoice, choice2, choice3];

(**********************************************************************)
(* ##########              Start the program.              ########## *)
(**********************************************************************)
condDriver3D[diffFunctionListINPUT, numDependentVariablesINPUT,
    independentVariableListINPUT, nameINPUT, noteINPUT, parametersINPUT,
    weightedParametersINPUT, rankRhoINPUT,
    explicitIndependentVariablesInDensitiesINPUT, formRhoINPUT,
    userWeightRulesINPUT]
]; (* end Module newMenu *)

(* ##########               Function: menu                 ########## *)

(**********************************************************************)
(* menu (no arguments)                                                *)
(* Purpose: This is the menu for the (1+1) cases from the old code.   *)
(* Input:   When program is first run, the menu is printed for user   *)
(*          to choose which case they want to run. (by choosing a     *)
(*          number from the list)                                     *)
(* Output:  Loads the chosen case.                                    *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whlemenu.m                                          *)
(* Code is in File:  conservationlaws/dplemenu.m                      *)
(* Major Changes Made: 26 May, 2006, 13:35 by DP at CSM               *)
(* Last Modified:  8 July, 2008, 18:48 by DP at CSM                   *)
(**********************************************************************)

(**********************************************************************)
(* printpage[n, page]: a subroutine for menu                          *)
(**********************************************************************)

printpage[n_, page_] := Module[{lenpage, choice},
  lenpage = Length[page[n]];
  Print[" "];
  Print["  *** MENU INTERFACE ***  (page: ", n, ")"];
  Print["-------------------------------------------"];
  For[i=1, i <= lenpage, i++,
    Print[Part[Part[page[n], i], 1]]];
  Print[" nn) Next Page"];
  Print[" qq) Exit the Program"];
  Print["------------------------------------------"];
  choice = Input["ENTER YOUR CHOICE: "];
  Return[choice]
]; (* end Module printpage *)

(**********************************************************************)
(* menu: creates the menu                                             *)
(**********************************************************************)

menu :=
Module[{counterpage = 1, menulist, numpages, page, choice1, control,
    lenmenulist, i},

   menulist = {
    {"  1) KdV Equation (d_kdv.m)"},
    {"  2) Generalized KdV Equation (d_gkdv.m)"},
    {"  3) Fifth Order KdV Equation (parameterized) (d_5kdv.m)"},
    {"  4) Fifth Order KdV Equation-Lax Case (d_5lax.m)"},
    {"  5) Fifth Order KdV Equation-Sawada-Kotera Case (d_5sk.m)"},
    {"  6) Fifth Order KdV Equation-Kaup-Kuperschmidt Case (d_5kk.m)"},
    {"  7) Fifth Order KdV Equation-Ito Case (d_5ito.m)"},
    {"  8) Seventh Order KdV Equation (parameterized) (d_7kdv.m)"},
    {"  9) Seventh Order KdV Equation-Lax Case (d_7lax.m)"},
    {" 10) Seventh Order KdV Equation-Sawada-Kotera-Ito Case (d_7ski.m)"},
    {" 11) Seventh Order KdV Equation Kaup-Kuperschmidt Case (d_7kk.m)"},
    {" 12) Schamel Equation (d_scham.m)"},
    {" 13) Rosenau-Hyman Equation (d_roshy.m)"},
    {" 14) Hirota-Satsuma System (parameterized) (d_phrsat.m)"},
    {" 15) Hirota-Satsuma System (d_hirsat.m)"},
    {" 16) Ito System (parameterized) (d_pito.m)"},
    {" 17) Ito System (d_ito.m)"},
    {" 18) NLS System (real and imaginary parts) (d_nls.m)"},
    {" 19) NLS System (equation and its conjugate) (d_nlsalt.m)"},
    {" 20) DNLS System (d_dnls.m)"},
    {" 21) MVDNLS System (d_mvdnls.m)"},
    {" 22) Kaup System (parameterized) (d_pkaup.m)"},
    {" 23) Kaup System (d_kaup.m)"},
    {" 24) Kaup-Broer System (d_broer.m)"},
    {" 25) Drinfel'd-Sokolov-Wilson System (d_soko.m)"},
    {" 26) Dispersive Long Wave System (d_disper.m)"},
    {" 27) Non-dispersive Long Wave System (d_nodisp.m)"},
    {" 28) 3-Component KdV System (d_3ckdv.m)"},
    {" 29) 2-Component NLS Equation (d_2cnls.m)"},
    {" 30) Boussinesq System (d_bous.m)"},
    {" 31) Riemann System (d_riemab.m)"},
    {" 32) Gardner Equation (d_gard.m)"},
    {" 33) Kawahara Equation (d_kawa.m)"},
    {" 34) Fokker-Plank Equation (d_fokkpl.m)"},
    {" 35) Fitzhugh-Nagumo Equation (d_fitzna.m)"},
    {" 36) Cahn-Hilliard Equation (d_cahnhi.m)"},
    {" 37) Chemotaxis Model (d_chemo.m)"},
    {" 38) Heat/Mass Transfer Equation (d_hematr.m)"},
    {" 39) Equations for Nonlinear Shallow Water Waves (d_nlssw.m)"}
  }; (* closes menulist *)

  lenmenulist = Length[menulist];
  numpages = Ceiling[lenmenulist/10];
  For[i = 1, i <= numpages, i++,
    page[i] = If[lenmenulist >= (i*10),
      menulist[[Table[k, {k, (i-1)*10+1, i*10}]]],
      menulist[[Table[k, {k, (i-1)*10+1, lenmenulist}]]]
      ] (* end If *)
  ]; (* end For *)

  choice1 = printpage[counterpage, page];

  control := (
    Switch[choice1,
      nn, If[counterpage < numpages, counterpage++;
          choice1 = printpage[counterpage, page]; control,
          counterpage = 1; choice1 = printpage[1, page]; control],
      qq, Print["All computations are being discontinued."]; Abort[],
       1, Get["d_kdv.m"],      2, Get["d_gkdv.m"],       3, Get["d_5kdv.m"],
       4, Get["d_5lax.m"],     5, Get["d_5sk.m"],        6, Get["d_5kk.m"],
       7, Get["d_5ito.m"],     8, Get["d_7kdv.m"],       9, Get["d_7lax.m"],
      10, Get["d_7ski.m"],    11, Get["d_7kk.m"],       12, Get["d_scham.m"],
      13, Get["d_roshy.m"],   14, Get["d_phrsat.m"],    15, Get["d_hirsat.m"],
      16, Get["d_pito.m"],    17, Get["d_ito.m"],       18, Get["d_nls.m"],
      19, Get["d_nlsalt.m"],  20, Get["d_dnls.m"],      21, Get["d_mvdnls.m"],
      22, Get["d_pkaup.m"],   23, Get["d_kaup.m"],      24, Get["d_broer.m"],
      25, Get["d_soko.m"],    26, Get["d_disper.m"],    27, Get["d_nodisp.m"],
      28, Get["d_3ckdv.m"],   29, Get["d_2cnls.m"],     30, Get["d_bous.m"],
      31, Get["d_riemab.m"],  32, Get["d_gard.m"],      33, Get["d_kawa.m"],
      34, Get["d_fokkpl.m"],  35, Get["d_fitzna.m"],    36, Get["d_cahnhi.m"],
      37, Get["d_chemo.m"],   38, Get["d_hematr.m"],    39, Get["d_nlssw.m"],
      _,  choice1 = Input["Please enter a choice from the menu."];
          control
    ]; (* closes Switch *)
  ); (* end control *)

  control;

  Check[control, Abort[]];

(* TO DO: 10/01/2002 *)
(* taken out during development, bring back later *)
(* If[Not[StringQ[myfile]],
   myfile = InputString["Enter the name of the output file:"]] *)

  (* Clear all local variables not being returned.                      *)
  Clear[counterpage, menulist, numpages, page, choice1, control,
      lenmenulist, i]
]; (* end Module menu *)

(* ##########              Function: menu3D                ########## *)

(**********************************************************************)
(* menu3D (no arguments)                                              *)
(* Purpose: display a menu of all the 3-D cases                       *)
(* Input:   when program is first run, the user chooses the multi-D   *)
(*          cases, and this menu is displayed so the user can choose  *)
(*          the specific case of interest                             *)
(* Output:  the chosen case is loaded                                 *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whmenu3d.m, Created 1 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpmenu3d.m                      *)
(* Last Modified:  20 July, 2009 12:40 by DP at CSM                   *)
(**********************************************************************)

menu3D :=
Module[{menulist, choice1, choice2, choice3, i, page, counterpage = 1,
    lenMenulist, numPages, control},

  menulist = {
  {"  1) (2+1) Zakharov-Kuznetsov (ZK) Equation (d_zk2d.m)"},
  {"  2) (3+1) Zakharov-Kuznetsov (ZK) Equation (d_zk3d.m)"},
  {"  3) (2+1) Kadomtsev-Petviashvili (KP) Equation (d_kp2d.m)"},
  {"  4) (2+1) Potential Kadomtsev-Petviashvili Equation (d_kp2dpo.m)"},
  {"  5) (2+1) KP Equation with extra y-derivative (d_kp2ddy.m)"},
  {"  6) (2+1) Kadomtsev-Petviashvili-B Equation (d_kpb2d.m)"},
  {"  7) (2+1) Alternative Kadomtsev-Petviashvili Equation (d_kp2dal.m)"},
  {"  8) (3+1) Kadomtsev-Petviashvili Equation (d_kp3d.m)"},
  {"  9) (3+1) Navier's Equation (d_navi3d.m)"},
  {" 10) (2+1) Gardner Equation (by Konopelchenko and Dubrovsky) (d_kd2d.m)"},
  {" 11) (2+1) Sawada-Kotera Equation (d_sk2d.m)"},
  {" 12) (2+1) Kaup-Kupershmidt Equation (d_kauk2D.m)"},
  {" 13) (2+1) Coupled Korteweg-de Vries System (d_kdv2d.m)"},
  {" 14) (2+1) Rotating Shallow Water Equations (d_rsw2d.m)"},
  {" 15) (2+1) Shallow Water Equations (Dellar) (d_dell2d.m)"},
  {" 16) (2+1) Shallow Water Magnetohydrodynamic Equations (d_de142d.m)"},
  {" 17) (2+1) Thermal Shallow Water Magnetohydrodynamic Eqns. (d_de342d.m)"},
  {" 18) (2+1) Khokhlov-Zabolotskaya (KZ) Equation (d_kz2d.m)"},
  {" 19) (2+1) Potential Khokhlov-Zabolotskaya Equation (d_kz2dpo.m)"},
  {" 20) (2+1) KZ Equation with extra y-derivative (d_kz2ddy.m)"},
  {" 21) (3+1) Khokhlov-Zabolotskaya Equation (d_kz3d.m)"},
  {" 22) (3+1) Potential Khokhlov-Zabolotskaya Equation (d_kz3d.m)"},
  {" 23) (2+1) Manakov-Santini System (d_ms2d.m)"},
  {" 24) (2+1) Camassa-Holm Equation (d_ch2d.m)"},
  {" 25) (2+1) Incompressible Euler Equations (d_eulr2d.m)"},
  {" 26) (3+1) Incompressible Euler Equations (d_eulr3d.m)"},
  {" 27) (2+1) Boussinesq Equation (d_bous2d.m)"},
  {" 28) (3+1) Burgers Equation (d_burg3d.m)"},
  {" 29) (2+1) Nonlinear Schrodinger Equation (d_nls2d.m)"},
  {" 30) (3+1) Nonlinear Schrodinger Equation (d_nls3d.m)"},
  {" 31) (3+1) Klein-Gordon Equation (d_klgo3d.m)"},
  {" 32) (2+1) Equations for Nonstationary Transonic Gas Flow (d_trag2d.m)"},
  {" 33) (3+1) Equations for Nonstationary Transonic Gas Flow (d_trag3d.m)"},
  {" 34) (3+1) Equations for Viscous Transonic Flow (d_visc3d.m)"},
  {" 35) (2+1) Equations Isentropic Motion of Inviscid Gas (d_invd2d.m)"},
  {" 36) (3+1) Equations Isentropic Motion of Inviscid Gas (d_invd3d.m)"},
  {" 37) (2+1) Equation for an Unsteady Hydronamic Layer (d_hydr2d.m)"},
  {" 38) (2+1) Equation for Heat Transfer (d_htra2d.m)"},
  {" 39) (2+1) Von-Karmen System (d_vkar2d.m)"},
  {" 40) (3+1) Wave Equation (d_wave3d.m)"}
  }; (* end of menulist *)

  lenMenulist = Length[menulist];
  numPages = Ceiling[lenMenulist/10];
  For[i=1, i<=numPages, i++,
     page[i] = If[lenMenulist >= (i*10),
                 menulist[[Table[k, {k, (i-1)*10+1, i*10}]]],
                 (* else *)
                 menulist[[Table[k, {k, (i-1)*10+1, lenMenulist}]]]
                 ]
     ];

  choice1 = printpage[counterpage, page];

    control := (
      Switch[choice1,
        nn, If[counterpage<numPages,
              counterpage++,
            (* else *)
              counterpage = 1;
            ];
            choice1 = printpage[counterpage,page];
            control,
        qq, Print["All computations are being discontinued"]; Abort[],
         1, Get["d_zk2d.m"],     2, Get["d_zk3d.m"],     3, Get["d_kp2d.m"],
         4, Get["d_kp2dpo.m"],   5, Get["d_kp2ddy.m"],   6, Get["d_kpb2d.m"],
         7, Get["d_kp2dal.m"],   8, Get["d_kp3d.m"],     9, Get["d_navi3d.m"],
        10, Get["d_kd2d.m"],    11, Get["d_sk2d.m"],    12, Get["d_kauk2d.m"],
        13, Get["d_kdv2d.m"],   14, Get["d_rsw2d.m"],   15, Get["d_dell2d.m"],
        16, Get["d_de142d.m"],  17, Get["d_de342d.m"],  18, Get["d_kz2d.m"],
        19, Get["d_kz2dpo.m"],  20, Get["d_kz2ddy.m"],  21, Get["d_kz3d.m"],
        22, Get["d_kz3dpo.m"],  23, Get["d_ms2d.m"],    24, Get["d_ch2d.m"],
        25, Get["d_eulr2d.m"],  26, Get["d_eulr3d.m"],  27, Get["d_bous2d.m"],
        28, Get["d_burg3d.m"],  29, Get["d_nls2d.m"],   30, Get["d_nls3d.m"],
        31, Get["d_klgo3d.m"],  32, Get["d_trag2d.m"],  33, Get["d_trag3d.m"],
        34, Get["d_visc3d.m"],  35, Get["d_poly2d.m"],  36, Get["d_poly3d.m"],
        37, Get["d_gasd2d.m"],  38, Get["d_hydr2d.m"],  39, Get["d_htra2d.m"],
        40, Get["d_vkar2d.m"],  41, Get["d_wave3d.m"],
        _,  choice1 = Input["Please enter a choice from the menu."];
            control
      ]; (* close switch *)
    ); (* close control *)

  control;

  Check[control, Abort[]];

  (* Clear all local variables not being returned.                      *)
  Clear[menulist, choice1, choice2, choice3, i, page, counterpage,
      lenMenulist, numPages, control]

]; (* end Module Menu3D *)

(* ##########              Function: pdeform               ########## *)

(* UG 06/06/2024 changed by Unal Goktas *)
(**********************************************************************)
(* pdeform[expres]                                                    *)
(* Purpose: Changes an expression: removes functional dependence,     *)
(*          and prints derivatives as subscripts.                     *)
(* Authors: Tracy Otto, Tony Miller, and U. Goktas                    *)
(* Input:   An expression                                             *)
(* Output:  Same expression with all of the changes made              *)
(* Created: 1995 at CSM                                               *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whlepedf.m, Created 1995 at CSM                     *)
(* Code is in File:  conservationlaws/dplepdef.m                      *)
(* Last Modified:  19 December, 2006, 11:40 by DP at CSM              *)
(* Last Modified:  6 June, 2024, 9:10 AM by U. Goktas in Houston      *)
(**********************************************************************)

If[$VersionNumber < 7.0,
   pdeform[expres_] := expres /. {
           Derivative[n__][u[k_]][x__] :>
                   SequenceForm[u, Subscript[k],Subscript[","],
                       Subscript[
                       SequenceForm @@ Flatten[Table[#[[1]], {#[[2]]}]& /@
                                           Transpose[{{x}, {n}}]]]],
           u[n_][x__] :> SequenceForm[u,Subscript[n]]
   },
   pdeform[expres_] := 
     expres /. {Derivative[n__][u[k_]][x__] :> 
        Subscript[u, k, 
         Row @@ {Flatten[
            Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}], 
       u[n_][x__] :> Subscript[u, n]}
] (* end pdeform *)


(* ##########       Function: printSystemInPdeForm         ########## *)

(**********************************************************************)
(* printSystemInPdeForm[diffFunctionList, name, note, numDepVars]     *)
(* Author: Mike Colagrosso                                            *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                3DCO0510.m                                          *)
(* Major Changes Made: 20 May, 2008, 16:04 by DP at CSM               *)
(**********************************************************************)

(* Major changes to function made since differential expressions      *)
(* and independent variables are passed by the function now.          *)
printSystemInPdeForm[diffFunctionList_, name_, note_, numDepVars_] :=
  Module[{inPDEform, eqnIter},
  Print[If[Length[diffFunctionList] > 1, "System given for ", ""],
      name, "."];
  If[Head[note] === String && note =!= "", Print["NOTE: ", note]];
  inPDEform = Map[pdeform, diffFunctionList];
  For[eqnIter = 1, eqnIter <= numDepVars, eqnIter++,
    If[eqnIter > 1, Print["\n Equation ",eqnIter," of the system"<>
        " with ",numDepVars," equations:"]];
    Print["\n  ", Part[inPDEform, eqnIter]," = 0\n"];
    ]; (* end For *)

  Clear[inPDEform, eqnIter]
]; (* end Module printSystemInPdeForm *)

(* ##########             Function: stripper               ########## *)

(**********************************************************************)
(* stripper[givenexpr, paramswithweight]                              *)
(* Purpose: Cancels the numerical factors of each term of the         *)
(*          argument and puts these into a list after cancellation    *)
(* Input:   An expression                                             *)
(*          A list of weighted parameters if there are any            *)
(* Output:  A list of the terms of the expression with all constant   *)
(*          coefficients removed.                                     *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whlstrip.m                                          *)
(* Code is in File:  conservationlaws/dpnstrip.m                      *)
(* Major Changes Made: 22 May, 2006, 14:30 by DP at CSM               *)
(* Last Modified:  29 September,�2009, 17:39 by DP at home            *)
(**********************************************************************)

Clear[stripper];

stripper[givenexpr_, paramswithweight_:{}] :=
Module[{strippedTermsList, expandExpr, expnList, termsNestedInFunctions,
    rules1, rules2, rules3, coefficientList, singleNoCoefficientList,
    expnFnList, possibleIndVarCombos, possibleDepVarCombos, permutDepVarCombos,
    possibleVarCombos, i1, n1, printSTRIP},

  If[debugStripper, printSTRIP = Print, Clear[printSTRIP], Clear[printSTRIP]];

  printSTRIP["debug code: STRIP, Function: stripper, File: dpnstrip.m"];

  printSTRIP["At STRIP IN, the expression given to the stripper:"];
  printSTRIP[givenexpr];
  printSTRIP["At STRIP IN, parameters given to the stripper:"];
  printSTRIP[paramswithweight];

  (* If input expression is zero, deal with it separately.              *)
  If[givenexpr === 0, strippedTermsList = {0};
      Return[strippedTermsList]];

  If[Not[MemberQ[givenexpr, u[_][__], {0, Infinity}]] &&
      Not[MemberQ[givenexpr, Derivative[__][u[_]][__], {0, Infinity}]],
    If[Not[MemberQ[givenexpr, n1_ /;
           MemberQ[{x, y, z, t}, n1], {0, Infinity}]],
       Return[{1}]
      ] (* end If Not MemberQ *)
    ]; (* end If Not MemberQ *)

  (* Input expression must be in expanded form.                         *)
  expandExpr = Expand[givenexpr];
  printSTRIP["At STRIP 1, the expression given to the stripper in"<>
      " expanded form:"];
  printSTRIP[expandExpr];

  (* Create a list of terms in the given expression.                    *)
  expnList = If[Head[expandExpr] === Plus, Apply[List, expandExpr],
      {expandExpr}, {expandExpr}];
  printSTRIP["At STRIP 2, a list of terms in the given expression is:"];
  printSTRIP[expnList];

  (* Separate the coefficients and parameters from the terms            *)
  expnFnList = Apply[Power, Map[FactorList, expnList], {2, 2}];
  printSTRIP["At STRIP 3, each term in expnList has been changed into a"<>
      " list of factors:"];
  printSTRIP[expnFnList];

  (* Find all possible combinations of independent variables and        *)
  (* dependent variables that could be nested inside functions.  This   *)
  (* includes u[_][__] and derivatives of u.                            *)
  possibleIndVarCombos = {{t}, {x}, {y}, {z}, {t, x}, {t, y}, {t, z}, {x, t},
      {x, y}, {x, z}, {y, t}, {y, x}, {y, z}, {z, t}, {z, x}, {z, y},
      {t, x, y}, {t, x, z}, {t, y, x}, {t, y, z}, {t, z, x}, {t, z, y},
      {x, t, y}, {x, t, z}, {x, y, t}, {x, y, z}, {x, z, t}, {x, z, y},
      {y, t, x}, {y, t, z}, {y, x, t}, {y, x, z}, {y, z, t}, {y, z, x},
      {z, t, x}, {z, t, y}, {z, x, t}, {z, x, y}, {z, y, t}, {z, y, x},
      {t, x, y, z}, {t, x, z, y}, {t, y, x, z}, {t, y, z, x}, {t, z, x, y},
      {t, z, y, x}, {x, t, y, z}, {x, t, z, y}, {x, y, t, z}, {x, y, z, t},
      {x, z, t, y}, {x, z, y, t}, {y, t, x, z}, {y, t, z, x}, {y, x, t, z},
      {y, x, z, t}, {y, z, t, x}, {y, z, x, t}, {z, t, x, y}, {z, t, y, x},
      {z, x, t, y}, {z, x, y, t}, {z, y, t, x}, {z, y, x, t}};

  (* Find all u[_] and Derivatives of u[_], even if they are nested in  *)
  (* a function. Find all other functions of the independent            *)
  (* variables.                                                         *)
  termsNestedInFunctions = Union[Flatten[Table[Map[Cases[expnFnList,
      Nest[_, #, i1] | _[Nest[_, #, i1], Nest[_, #, i1]] |
      _[Nest[_, #, i1], Nest[_, #, i1], Nest[_, #, i1]] |
      _[Nest[_, #, i1], Nest[_, #, i1], Nest[_, #,i1], Nest[_, #, i1]] /.
      List -> Sequence, {0, Infinity}] &, possibleIndVarCombos], {i1, 1, 4}]],
      Flatten[Map[Cases[expnFnList, Power[_, _[#]] | _[Power[_, _[#]]] /.
      List -> Sequence, {0, Infinity}] &, possibleIndVarCombos]]];
  printSTRIP["At STRIP 4, all terms containing u[_], derivatives of u[_],"<>
      " or functions of the independent variables:"];
  printSTRIP[termsNestedInFunctions];

  (* If rational expressions have dependent variables in the            *)
  (* denominator, zeros can occur with replacement                      *)
  Off[Power::infy];

  (* Create a list of coefficients - not including any weighted         *)
  (* parameters. Two rules are developed to remove all unwanted parts.  *)
  rules1 = Map[Rule[#, {}] &, Flatten[termsNestedInFunctions]];
  rules2 = Apply[List, Map[Rule[#, {}] &, paramswithweight]];
  rules3 = {x -> {}, y -> {}, z -> {}, t -> {}};
  coefficientList = expnFnList /. rules1;
  printSTRIP["At STRIP 5, dependent variables and the derivatives of"<>
      " dependent variables have been changed to {}:"];
  printSTRIP[coefficientList];

  coefficientList = coefficientList /. rules2;
  printSTRIP["At STRIP 6, any weighted parameters have been changed to {}:"];
  printSTRIP[coefficientList];

  coefficientList = coefficientList /. rules3;
  printSTRIP["At STRIP 7, explicit independent variables, x, y, z, or t,"<>
      " have been changed to {}:"];
  printSTRIP[coefficientList];

  (* The {} is used first to avoid getting zeros in denominators when   *)
  (* rational terms are given in the problem.                           *)
  coefficientList = coefficientList /. {} -> 1;
  printSTRIP["At STRIP 8, a list of coefficients where {} is changed to 1:"];
  printSTRIP[coefficientList];

  (* Combine the coefficients for each term.                            *)
  singleNoCoefficientList = Apply[Times, coefficientList, {1, 1}];
  printSTRIP["At STRIP 9, a combined coefficient list is:"];
  printSTRIP[singleNoCoefficientList];

  (* Form a list of terms without coefficients, but including weighted  *)
  (* parameters.                                                        *)
  strippedTermsList = If[Length[expnFnList] > 1,
      Thread[Times[expnList, 1/singleNoCoefficientList]],
      expnList/singleNoCoefficientList];
  strippedTermsList = Factor[strippedTermsList];
  printSTRIP["At STRIP 10, a list of stripped terms is:"];
  printSTRIP[strippedTermsList];

  (* Clear all local variables not being returned.                      *)
  Clear[expandExpr, expnList, rules1, rules2, rules3, coefficientList,
      singleNoCoefficientList, possibleIndVarCombos, possibleDepVarCombos,
      permutDepVarCombos, possibleVarCombos,expnFnList, n1, printSTRIP];

  Return[strippedTermsList];
]; (* end Module stripper *)

(* ##########         Function: putInEvolutionForm         ########## *)

(**********************************************************************)
(* putInEvolutionForm[equationList, formOfRho, noOfDepVars,           *)
(*     preferredEvolutionIndVar, pdename]                             *)
(* Purpose: To take a given PDE and rewrite it into evolution form    *)
(*          if possible.  Program reports if evolution form is not    *)
(*          possible.                                                 *)
(* Input:   Equation or list of equations                             *)
(*          A density provided by the user (if given)                 *)
(*          Number of dependent variables                             *)
(*          The independent variable that is preferred for the        *)
(*              evolution variable when there is a choice             *)
(*          The name given for the PDE                                *)
(* Output:  Equation or system in evolution form                      *)
(*          The number of dependent variables in the evolution system *)
(*          Rules for reversing the changes in the density and flux   *)
(*              after calculations are finished                       *)
(* Created: 11 December, 2007 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwpevofo.m                      *)
(* Last Modified:  3 June, 2009, 17:32 by DP at CSM                   *)
(**********************************************************************)

putInEvolutionForm[equationList_List, formOfRho_, noOfDepVars_Integer,
    preferredEvolutionIndVar_, pdename_] :=
Module[{lengthEqnList, preferred, match1, i1, m1, m2, m3, m4, n1, n2, n3,
    n4, nn, strippedTerms, possibleEvoDerivs, indepVars, factoredEqnList,
    standAloneTerms, indVarRule, derivPattern, evoVar, varList, stop = 0,
    allEvoDerivs, newDerivPattern, maxEvoVar, depVarCheck, currentTerm,
    currentDepVar, noDerivsOnEvoVar, newDepVar = noOfDepVars, eqnCt = 1,
    derivEvoSet, ptn, evoEqnList = {}, newEqn, undoNewDepVar = {},
    evoDerivs, derivEnds, conds, derivChangeRule, ddd, rul, evoFormOfRho,
    leftOverIndVars, transformRuleLHS, transformRuleRHS, transformRule,
    reverseTransformRuleLHS, reverseTransformRuleRHS, printPEF,
    reverseTransformRule, lthsys, evolutionTerms, positionEvoTerms,
    addltDerTerms, positionOrigEvo, newEvoRules, listOfDersNeeded,
    lhsReplaceRules, rhsReplaceRules, newReplaceRules, evoEqnListAlt},

If[debugPutInEvolutionForm, printPEF = Print, Clear[printPEF],
    Clear[printPEF]];

  printPEF["debug code: PEF, Function: putInEvolutionForm,"<>
      " File: nwpevofo.m"];

  lengthEqnList = If[Head[Part[equationList, 1]] === Plus,
      Length[equationList], 1];

  printPEF["At PEF IN, the "<> If[lengthEqnList === 1,
      "equation: ", "system of equations: "], equationList];
  printPEF["At PEF IN, the user has provided the density:"];
  printPEF[formOfRho];
  printPEF["There are ", noOfDepVars, " dependent variables"];

  preferred = If[Head[preferredEvolutionIndVar] === List,
      Part[preferredEvolutionIndVar, 1], preferredEvolutionIndVar];
  match1 = Table[1, {i1, 1, lengthEqnList}];

  (* Part 1: Search for an independent variable that can act as an      *)
  (* evolution variable.                                                *)
  strippedTerms = Map[stripper, equationList];
  printPEF["At PEF 1, stripped terms for each equation are given in"<>
      " individual lists:"];
  printPEF[strippedTerms];

  (* Find the independent variables as they are listed in the           *)
  (* expression.                                                        *)
  indepVars = Union[Cases[strippedTerms, Derivative[__][u[_]][__],
      {0, Infinity}], Cases[strippedTerms, u[_][__], {0, Infinity}]];
  printPEF["At PEF 2, all terms containing u and derivatives of u:"];
  printPEF[indepVars];

  indepVars = Union[indepVars /. {Derivative[__][u[_]][n1__] -> {n1},
      u[_][n2__] -> {n2}}];
  printPEF["At PEF 3, a list of independent variables taken from the terms:"];
  printPEF[indepVars];

  indepVars = If[Length[indepVars] > 1,
    Print["The independent variables have not been placed on the terms"<>
        " in a consistent manner."];
    Print["All computations are being discontinued."];
    Abort[],
  (* else *)
    Flatten[indepVars]
  ]; (* end If Length[iindepVars] *)

  factoredEqnList = Map[FactorList, strippedTerms, {2}];
  printPEF["At PEF 4, the list of stripped terms after applying FactorList:"];
  printPEF[factoredEqnList];

  (* Check the terms for a variable that stands alone in the equation.  *)
  standAloneTerms = Map[Cases[#, {{1, 1}, {__}}, {0, Infinity}] &,
      factoredEqnList];
  printPEF["At PEF 5, all terms that do not sit in products:"];
  printPEF[standAloneTerms];

  standAloneTerms = DeleteCases[standAloneTerms,
      {{1, 1}, {_, n1_}} /; n1 =!= 1, {0, Infinity}];
  printPEF["At PEF 6, all terms that do not sit in powers:"];
  printPEF[standAloneTerms];

  standAloneTerms = Apply[Times, Apply[Power, standAloneTerms, {3}], {2}];
  printPEF["At PEF 7, after undoing the effects of FactorList on the"<>
      " remaining terms:"];
  printPEF[standAloneTerms];

  indVarRule = Union[Map[Rule[#, 0] &,
      Complement[indepVars, {preferred}]], {Rule[preferred, n1_]}];
  printPEF["At PEF 8, the first search to find an independent variable to"<>
      " form evolution rules on looks for terms with derivatives of"<>
      " ", preferred, " only:"];
  printPEF[indVarRule];

  derivPattern = indepVars /. indVarRule;
  evoVar = preferred;
  varList = indepVars;

  While[stop === 0 && varList =!= {},
    printPEF["At PEF 9, the pattern for derivatives to search for:"];
    printPEF[derivPattern];

    possibleEvoDerivs = Map[Cases[#, Derivative[Apply[Sequence, derivPattern]]
        [u[_]][__]] &, standAloneTerms];
    printPEF["At PEF 10, a list of possible evolution terms with"<>
        " ", evoVar, " as the evolution variable:"];
    printPEF[possibleEvoDerivs];

    If[Map[Length, possibleEvoDerivs] === match1,
      newDerivPattern = derivPattern /. 0 -> _;
      allEvoDerivs = Map[Cases[#, Derivative[
          Apply[Sequence, newDerivPattern]][u[_]][__] /; n1 > 0,
          {0, Infinity}] &, equationList];
      printPEF["At PEF 11, all terms with "<>ToString[evoVar]<>
          "-derivatives:"];
      printPEF[allEvoDerivs];

      maxEvoVar = allEvoDerivs /. Derivative[
          Apply[Sequence, newDerivPattern]][u[_]][__] -> n1;
      printPEF["At PEF 12, a list of the number of "<>ToString[evoVar]<>
          "-derivatives in each term of the list in PEF 11:"];
      printPEF[maxEvoVar];

      maxEvoVar = Flatten[Thread[position[maxEvoVar, Map[Max, maxEvoVar]]] /.
          position -> Position, 1];
      printPEF["At PEF 13, the position of the greatest number of "<>
          ToString[evoVar]<>"-derivatives:"];
      printPEF[maxEvoVar],

    (* else *)
      maxEvoVar = {}
    ]; (* end If Map Length *)

    If[Map[Length, maxEvoVar] === match1,
      possibleEvoDerivs =Thread[ext[allEvoDerivs, maxEvoVar]] /. ext -> Extract;
      printPEF["At PEF 14, the evolution terms are:"];
      printPEF[possibleEvoDerivs];

      depVarCheck = Flatten[possibleEvoDerivs /.
          Derivative[__][u[n1_]][__] -> n1];
      printPEF["At PEF 15, the dependent variables in the evolution terms:"];
      printPEF[depVarCheck];

      stop = If[Sort[depVarCheck] === Range[1, noOfDepVars], 1, 0]
    ]; (* end If Map Length *)

    If[stop === 0,
      derivPattern = RotateLeft[derivPattern];
      evoVar = Part[Extract[indepVars,
          {{Part[Position[derivPattern, n1], 1, 1]}}], 1]
    ]; (* end If stop *)

    varList = Drop[varList, 1];

  ]; (* end While *)

  If[stop === 0,
    Print["The program is unable to find an evolution form for this"<>
        " equation.  It may be helpful if a piece is assigned to a new"<>
        " dependent variable and the system is resubmitted to the program."];
    Print["All computations are being discontinued."];
    Abort[]
  ]; (* end If stop *)

  While[possibleEvoDerivs =!= {},
    currentTerm = First[possibleEvoDerivs];
    printPEF["At PEF 16, the current evolution term is:"];
    printPEF[currentTerm];

    currentDepVar = currentTerm /. Derivative[__][u[n2_]][__] -> n2;
    printPEF["At PEF 17, the dependent variable of the current evolution"<>
        " term is:"];
    printPEF[currentDepVar];

    noDerivsOnEvoVar = currentTerm /.
        Derivative[Apply[Sequence, derivPattern]][_][__] -> n1;
    printPEF["At PEF 18, the number of derivatives on the evolution variable"<>
        " in the evolution term is:"];
    printPEF[noDerivsOnEvoVar];

    (* Form a system of equations when the evolution variable has a     *)
    (* higher order.                                                    *)
    While[noDerivsOnEvoVar > 1,
      newDepVar = newDepVar + 1;
      derivEvoSet = derivPattern /. Pattern -> ptn /. ptn[n1, Blank[]] -> 1;
      newEqn[eqnCt] = u[newDepVar][Apply[Sequence, indepVars]] -
          Derivative[Apply[Sequence, derivEvoSet]][u[currentDepVar]]
          [Apply[Sequence, indepVars]];
      printPEF["At PEF 19, a new equation for the system is:"];
      printPEF[newEqn[eqnCt], " == 0"];

      evoEqnList = Append[evoEqnList, newEqn[eqnCt]];
      printPEF["At PEF 20, the list of equations for the evolution system"<>
          " now contains:"];
      printPEF[evoEqnList];

      undoNewDepVar = Append[undoNewDepVar, newEqn[eqnCt]];
      currentDepVar = newDepVar;
      noDerivsOnEvoVar--;
      eqnCt++
    ]; (* end While noDerivsOnEvoVar *)

    possibleEvoDerivs = Drop[possibleEvoDerivs, 1]
  ]; (* end While possibleEvoTerms *)

  (* All terms containing the original dependent variables must have    *)
  (* the evolution variable derivatives changed according to the new    *)
  (* system of equations.                                               *)
  evoDerivs = derivPattern /. Pattern -> ptn /. ptn[__] -> 1;
  printPEF["At PEF 21, the basic derivative set for the evolution term:"];
  printPEF[evoDerivs];

  derivEnds = ToExpression[Table["n"<>ToString[i], {i, 1, Length[evoDerivs]}]];
  printPEF["At PEF 22, the end pattern for derivatives:"];
  printPEF[derivEnds];

  conds = Apply[And, Thread[GreaterEqual[derivEnds, evoDerivs]]];
  printPEF["At PEF 23, conditions on changing the derivatives:"];
  printPEF[conds];

  derivPattern = Map[Pattern[#, Blank[]] &, derivEnds];
  printPEF["At PEF 24, the front derivative pattern:"];
  printPEF[derivPattern];

  derivChangeRule = Table[Rule[-Part[newEqn[i1], 2], Part[newEqn[i1], 1]],
      {i1, 1, eqnCt - 1}] /. Rule -> rul /.
      {Derivative[Apply[Sequence, evoDerivs]][m1_][m2__] ->
         Derivative[Apply[Sequence, derivPattern]][m1][m2] /; Evaluate[conds],
      u[m3_][m4__] ->  ddd[u[m3][m4],
         Apply[Sequence, Thread[List[indepVars, derivEnds - evoDerivs]]]]} /.
      ddd -> D /. rul -> Rule;
  printPEF["At PEF 25, the rule for changing all terms with "<>
      ToString[evoVar]<>"-derivatives that are not evolution terms:"];
  printPEF[derivChangeRule];

  (* Rewrite the original equation(s) in evolution form.                *)
  evoEqnList = equationList //. derivChangeRule;
  printPEF["At PEF 26, differential function list has been changed using"<>
      " the rule in PEF 25:"];
  printPEF[evoEqnList];

  evoEqnList = Union[evoEqnList, Table[newEqn[i1], {i1, 1, eqnCt - 1}]];
  printPEF["At PEF 27,a list of equations in evolution form with the"<>
      " variable "<>ToString[evoVar]<>":"];
  printPEF[evoEqnList];

  (* Apply changes to a form of the density given by the user.          *)
  evoFormOfRho = formOfRho //. derivChangeRule;
  printPEF["At PEF 28, form of the density given by the user has been"<>
      " changed using the rule in PEF 25:"];
  printPEF[evoFormOfRho];

  (* Part 2: If the evolution variable does not match the variable      *)
  (* preferred to be the evolution variable, transform the PDE by doing *)
  (* an independent variable exchange.                                  *)

  If[evoVar =!= preferred,
    leftOverIndVars = Complement[indepVars, {evoVar, preferred}];
    printPEF["At PEF 29, independent variables NOT playing a role in the"<>
        " evolution form:"];
    printPEF[leftOverIndVars];

    transformRuleLHS = indepVars /. Flatten[{evoVar -> nn1_, preferred -> nn2_,
        Thread[Rule[leftOverIndVars, Table[ToExpression[StringJoin["nn",
        ToString[i], "_"]], {i, 3, Length[leftOverIndVars] + 2}]]]}];
    printPEF["At PEF 30, the left hand side of the rule for transforming"<>
        " the independent variables:"];
    printPEF[transformRuleLHS];

    transformRuleRHS = transformRuleLHS /. Pattern -> ptn /.
        {ptn[nn1, _] -> nn2, ptn[nn2, _] -> nn1,
        ptn[nn_, _] /; nn =!= nn1 && nn =!= nn2 -> nn};
    printPEF["At PEF 31, the right hand side of the rule for transforming"<>
        " the independent variables:"];
    printPEF[transformRuleRHS];

    transformRule =
        Rule[Derivative[Apply[Sequence, transformRuleLHS]][m1_][m2__],
        Derivative[Apply[Sequence, transformRuleRHS]][m1][m2]];
    printPEF["At PEF 32, the rule for transforming the independent"<>
        " variables:"];
    printPEF[transformRule];

    evoEqnList = evoEqnList /. transformRule;
    printPEF["At PEF 33, the list of evolution equations after the variable"<>
        " interchange has taken place:"];
    printPEF[evoEqnList];

    evoFormOfRho = evoFormOfRho /. transformRule;
    printPEF["At PEF 34, the form of the density given by the user after"<>
        " the variable interchange has taken place:"];
    printPEF[evoFormOfRho];

    (* Search for additional t-derivatives that need to be replaced     *)
    (* using one of the new evolution forms.                            *)
    tDerTerms = Map[Cases[#, Derivative[__, n1_][u[_]][__] /; n1 > 0,
       {0, Infinity}] &, evoEqnList];
    printPEF["At PEF 35, all terms with t-derivatives after the variable"<>
        " interchange has taken place:"];
    printPEF[tDerTerms];

    evolutionTerms = Cases[Flatten[tDerTerms],
        Derivative[Apply[Sequence, Table[0, {i1, 1, Length[indepVars]-1}]], 1]
        [u[_]][__], {0, Infinity}];
    printPEF["At PEF 36, all terms in evolution form in the system:"];
    printPEF[evolutionTerms];

    If[Complement[Flatten[tDerTerms], evolutionTerms] =!= {},
      positionEvoTerms = Map[Position[tDerTerms, #] &, Union[evolutionTerms]];
      positionEvoTerms = Map[First, positionEvoTerms];
      printPEF["At PEF 37, the position of the evolution terms in the list"<>
          " of t-derivatives:"];
      printPEF[positionEvoTerms];

      addltDerTerms = Flatten[ReplacePart[tDerTerms, {}, positionEvoTerms]];
      printPEF["At PEF 38, all terms that have t-derivatives that need to"<>
          " be replaced:"];
      printPEF[addltDerTerms];

      positionOrigEvo = Map[Position[tDerTerms, #] &, Cases[tDerTerms, n1_ /;
          Length[n1] === 1]];
      positionOrigEvo = Flatten[positionOrigEvo, 1];
      printPEF["At PEF 39, the position of evolution terms in the list"<>
          " with the interchanged variables:"];
      printPEF[positionOrigEvo];

      newEvoRules = Flatten[Solve[Map[Equal[0, #] &,
          Extract[evoEqnList, positionOrigEvo]], evolutionTerms]];
      printPEF["At PEF 40, new evolution rules which still need to be"<>
          " generalized:"];
      printPEF[newEvoRules];

      listOfDersNeeded = Thread[List[indepVars, derivEnds]] /.
          {t, m1_} -> {t, m1 - 1};
      printPEF["At PEF 41, list of derivatives with respect to the"<>
          " independent variables that need to be taken on the right hand"<>
          " side of the evolution rules:"];
      printPEF[listOfDersNeeded];

      depVarsInReplRules = Map[Part[#, 1] &, newEvoRules] /.
          Derivative[__][u[n1_]][__] -> n1;
      printPEF["At PEF 42, the new evolution rules are on dependent"<>
          " variables:"];
      printPEF[depVarsInReplRules];

      lhsReplaceRules = Map[Derivative[Apply[Sequence, derivPattern]]
          [u[#]][Apply[Sequence, indepVars]] /; m2 > 0 &, depVarsInReplRules];
      printPEF["At PEF 43, the left hand side of the generalized evolution"<>
          " rule:"];
      printPEF[lhsReplaceRules];

      rhsReplaceRules = Union[Map[Part[#, 2] &, newEvoRules]];
      rhsReplaceRules = Map[D[#, Apply[Sequence, listOfDersNeeded]] &,
          rhsReplaceRules];
      printPEF["At PEF 44, the right hand side of the generalized evolution"<>
          " rule:"];
      printPEF[rhsReplaceRules];

      newReplaceRules = Thread[rul[lhsReplaceRules, rhsReplaceRules]] /.
          {ddd -> D, rul -> Rule, m2 -> Last[derivEnds]};
      printPEF["At PEF 45, the new generalized evolution rules:"];
      printPEF[newReplaceRules];

      evoEqnListAlt = Delete[evoEqnList, positionOrigEvo];
      printPEF["At PEF 46, equations in the system for which the new evolution"<>
         " rules must be applied:"];
      printPEF[evoEqnListAlt];

      evoEqnListAlt = evoEqnListAlt /. newReplaceRules;
      printPEF["At PEF 47, all equations are now in evolution form:"];
      printPEF[evoEqnListAlt];

      evoEqnList = Flatten[Union[Extract[evoEqnList, positionOrigEvo],
          evoEqnListAlt]];
      printPEF["At PEF 48, equations in the system after the new evolution"<>
          " rules have been applied:"];
      printPEF[evoEqnList]
    ]; (* end If tDerTerms *)

  reverseTransformRuleLHS = Map[Pattern[#, Blank[]] &, transformRuleRHS];
   reverseTransformRuleRHS = transformRuleLHS /. Pattern -> ptn /.
        ptn[nn_, Blank[]] -> nn;
    reverseTransformRule =
        Rule[Derivative[Apply[Sequence, reverseTransformRuleLHS]][m1_][m2__],
      Derivative[Apply[Sequence, reverseTransformRuleRHS]][m1][m2]];
    printPEF["At PEF 49, the rule for reversing the exchange process:"];
    printPEF[reverseTransformRule],

  (* else *)
    reverseTransformRule = {}
  ]; (* end If evoVar === preferred *)

  (* Make a final check on changes.                                     *)
  stop = checkForEvolutionForm[evoEqnList, DeleteCases[indepVars, t]];

  If[stop === "No",
    Print["The program is unable to find an evolution form for this"<>
        " equation.  It may be helpful if a piece is assigned to a new"<>
        " dependent variable and the system is resubmitted to the program."];
    Print["All computations are being discontinued."];
    Abort[]
  ]; (* end If stop *)


  (* Print a report outlining the changes made.                         *)
  lthsys = Length[evoEqnList];
  printLOW["The " <> pdename <> " is not in evolution form.  An evolution\n" <>
      If[lthsys === 1, "equation", "system of equations"] <> " can be" <>
      " formed by " <> If[evoVar =!= t, "interchanging t and "<>
      ToString[evoVar] <>" with each other and by ", ""] <>"replacing\neach ",
      Subscript["u", "t"], " with a new dependent variable for each"<>
      " t-derivative in the term until\nonly single t-derivatives exist"<>
      " in a new system of equations.  The\n"<> pdename <>" is rewritten"<>
      " in evolution form as follows:"];
  Print["------------------------------------------"];

  (* Clear all local variables not being returned.                      *)
  Clear[lengthEqnList, preferred, match1, i1, m1, m2, m3, m4, n1, n2, n3,
      n4, nn, strippedTerms, possibleEvoDerivs, indepVars, factoredEqnList,
      standAloneTerms, indVarRule, derivPattern, evoVar, varList, stop,
      allEvoDerivs, newDerivPattern, maxEvoVar, depVarCheck, currentTerm,
      currentDepVar, noDerivsOnEvoVar, eqnCt, derivEvoSet, ptn, newEqn,
      evoDerivs, derivEnds, conds, derivChangeRule, ddd, rul, lthsys,
      leftOverIndVars, transformRuleLHS, transformRuleRHS, transformRule,
      printPEF, lthsys, evolutionTerms, positionEvoTerms,
      addltDerTerms, positionOrigEvo, newEvoRules, listOfDersNeeded,
      lhsReplaceRules, rhsReplaceRules, newReplaceRules, evoEqnListAlt];

  Return[{evoEqnList, evoFormOfRho, reverseTransformRule, undoNewDepVar,
      newDepVar}]
] (* end Module putInEvolutionForm *)

(* ##########        Function: checkForEvolutionForm       ########## *)

(**********************************************************************)
(* checkForEvolutionForm[diffFunctionList, indVars]                   *)
(* Purpose: To check a set of differential equations to see if each   *)
(*          equation is in evolution form                             *)
(* Input:   List of differential functions                            *)
(*          List of independent variables                             *)
(* Output:  Yes or No                                                 *)
(* Created: 12 December, 2007 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwpevofo.m                      *)
(* Last Modified:  20 May, 2008, 14:40 by DP at CSM                   *)
(**********************************************************************)

checkForEvolutionForm[diffFunctionList_List, indVars_] :=
Module[{zeroList, evolutionTerms, nonevolutionTerms, i, ans},

  If[debugCheckForEvolutionForm, printCFEF = Print, Clear[printCFEF],
      Clear[printCFEF]];

  printCFEF["debug code: CFEF, Function: checkForEvolutionForm,"<>
      " File: nwpevofo.m"];

  printCFEF["Entering checkForEvolutionForm with the differential function"<>
      "list:"];
  printCFEF[diffFunctionList];
  printCFEF["The independent variable list contains ", indVars];

  zeroList = Table[0, {i, 1, Length[indVars]}];
  printCFEF["At CFEF 1, the zero list contains:"];
  printCFEF[zeroList];

  evolutionTerms = Map[Cases[#,
      Derivative[Apply[Sequence, zeroList], 1][_][__], {0, Infinity}] &,
      diffFunctionList];
  printCFEF["At CFEF 2, a list of terms with single t-derivatives from each"<>
      " equation:"];
  printCFEF[evolutionTerms];

  nonevolutionTerms = Cases[diffFunctionList, Derivative[n1__, n2_][_][__] /;
      n2 > 1 || ({n1} =!= zeroList && n2 === 1), {0, Infinity}];
  printCFEF["At CFEF 3, a list of terms with t-derivatives not in"<>
      " evolution form:"];
  printCFEF[nonevolutionTerms];

  ans = If[Cases[evolutionTerms, {}] === {} && nonevolutionTerms === {},
     "Yes", "No"];

  (* Clear all local variables not being returned.                      *)
  Clear[zeroList, evolutionTerms, nonevolutionTerms, i];

  Return[ans]
] (* end Module checkForEvolutionForm *)

(* ##########       Function: reverseEvolutionChanges      ########## *)

(**********************************************************************)
(* reverseEvolutionChanges[listDensities, listFluxes,                 *)
(*     reverseRulesExch, reverseRulesSys, indepVars]                  *)
(* Purpose: To remove the changes made to write the original PDE in   *)
(*          evolution form from the results                           *)
(* Input:   List of densities                                         *)
(*          List of fluxes                                            *)
(*          Reversing rules for independent variable exchanges        *)
(*          Reversing equations for adding extra dependent variables  *)
(*          List of independent variables                             *)
(* Output:  Densities and fluxes in the form of the original PDE      *)
(* Created: 12 December, 2007 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwrevech.m                      *)
(* Last Modified:  24 June, 2009, 18:52 by DP at CSM                  *)
(**********************************************************************)

reverseEvolutionChanges[listDensities_, listFluxesIn_, reverseRulesExch_,
    reverseRulesSys_, indepVars_] :=
Module[{m1, n4, revisedIndepVars, undoRule, noDerivRule, listFluxes,
    derVarLHS, derVarRHS, derivRuleLHS, derivRuleRHS, derivRule, posSwapVar,
    swapVar, indepVarSwapRule1, indepVarSwapRule2, tempt, revisedRhos,
    dummyIndVars, der, revisedFluxes, pieceFlux, density, newFlux,
    compatibility, newDensity, tim, checkDivergences, cd, printREC},

  If[debugReverseEvolutionChanges, printREC = Print, Clear[printREC],
      Clear[printREC]];

  printREC["debug code: REC, Function: reverseEvolutionChanges,"<>
      " File: nwrevech.m"];

  printREC["At REC IN, the list of densities given:"];
  printREC[listDensities];
  printREC["At REC IN, the list of fluxes:"];
  printREC[listFluxesIn];
  printREC["At REC IN, the reversing rules for independent variable"<>
      " interchanges:"];
  printREC[reverseRulesExch];
  printREC["At REC IN, the reversing equations for adding extra"<>
      " dependent variables:"];
  printREC[reverseRulesSys];
  printREC["At REC IN, the independent variable list contains ", indepVars];

  listFluxes = If[Length[indepVars] === 1, 
      Thread[List[listFluxesIn]], listFluxesIn];
  revisedIndepVars = Append[indepVars, t];
  printREC["At REC 0, the list of fluxes has been revised to:"];
  printREC[listFluxes];
  printREC["The modified independent variables list is ", revisedIndepVars];

  (* Finish setting up the rules to reverse the effects of additional   *)
  (* dependent variables.                                               *)
  undoRule = Apply[List, reverseRulesSys, {1}] /.
      Times[-1, n1__] -> Times[1, n1];
  printREC["At REC 1, parts of added evolution equations are separated and"<>
      " minus signs are removed:"];
  printREC[undoRule];

  noDerivRule = Apply[Rule, undoRule, {1}];
  printREC["At REC 2, the rule for terms not containing derivatives:"];
  printREC[noDerivRule];

  derVarLHS = revisedIndepVars /. {x -> {x, n1_}, y -> {y, n2_},
       z -> {z, n3_}, t -> {t, n4_}};
  derVarRHS = revisedIndepVars /. {x -> {x, n1}, y -> {y, n2},
       z -> {z, n3}, t -> {t, n4}};
  derivRuleLHS = Map[D[Part[#, 1], Apply[Sequence, derVarLHS]] &, undoRule];
  derivRuleRHS = Map[D[Part[#, 2], Apply[Sequence, derVarRHS]] &, undoRule];
  derivRule = Thread[Rule[derivRuleLHS, derivRuleRHS]];
  printREC["At REC 3, the rule for terms containing derivatives:"];
  printREC[derivRule];

  If[reverseRulesExch =!= {},
    posSwapVar = Part[reverseRulesExch, 2] /. Derivative[m1__][_][__] -> {m1};
    posSwapVar = Part[Position[posSwapVar, nn1], 1, 1];
    swapVar = Part[indepVars, posSwapVar];
    printREC["At REC 4, the independent variable swapped with t is ", swapVar];

    indepVarSwapRule1 = t -> tempt;
    indepVarSwapRule2 = {swapVar -> t, tempt -> swapVar},
  (* else *)
    indepVarSwapRule1 = indepVarSwapRule2 = {}
  ]; (* end If reverseRulesExch *)
  printREC["At REC 5, the swap rules for the independent variables:"];
  printREC["indepVarSwapRule1 = ", indepVarSwapRule1,
      "\nindepVarSwapRule2 = ", indepVarSwapRule2];

  (* Undo all changes.                                                  *)
  revisedRhos = listDensities /. reverseRulesExch /.
      noDerivRule //. derivRule;
  printREC["At REC 7, the density list after added auxillary dependent"<>
      " variables have been removed:"];
  printREC[revisedRhos];

  revisedRhos = revisedRhos /.
      {Derivative[n1__][n2_][__] -> Derivative[n1][n2][dummyIndVars],
      u[n1_][__] -> u[n1][dummyIndVars]};
  printREC["At REC 8, the density list after all function lists of"<>
      " independent variables have been temporarily replaced:"];
  printREC[revisedRhos];

  revisedRhos = revisedRhos /. indepVarSwapRule1 /. indepVarSwapRule2;
  printREC["At REC 9, the density list after independent variable"<>
      " exchanges have been made:"];
  printREC[revisedRhos];

  revisedRhos = revisedRhos /.
      dummyIndVars -> Apply[Sequence, revisedIndepVars] /. der -> Derivative;
  printREC["At REC 10, the density list after all changes have been made:"];
  printREC[revisedRhos];

  revisedFluxes = listFluxes /. reverseRulesExch /.
      noDerivRule //. derivRule;
  printREC["At REC 11, the flux list after added auxillary dependent"<>
      " variables have been removed:"];
  printREC[revisedFluxes];

  revisedFluxes = revisedFluxes /.
      {Derivative[n1__][n2_][__] -> Derivative[n1][n2][dummyIndVars],
      u[n1_][__] -> u[n1][dummyIndVars]};
  printREC["At REC 12, the density list after all function lists of"<>
      " independent variables have been temporarily replaced:"];
  printREC[revisedRhos];

  revisedFluxes = revisedFluxes /. indepVarSwapRule1 /. indepVarSwapRule2;
  printREC["At REC 13, the flux list after independent variable"<>
      " exchanges have been made:"];
  printREC[revisedFluxes];

  revisedFluxes = revisedFluxes /.
      dummyIndVars -> Apply[Sequence, revisedIndepVars] /. der -> Derivative;
  printREC["At REC 14, the flux list after all changes have been made:"];
  printREC[revisedFluxes];

  (* Undo changes from an exchange of independent variables.  This      *)
  (* includes swapping a component of the flux with the density if t    *)
  (* is involved.                                                       *)
  If[reverseRulesExch =!= {},
    Clear[pieceFlux, density];
    newFlux = indepVars /. Part[indepVars, posSwapVar] -> density /.
        {x -> pieceFlux[1], y -> pieceFlux[2], z -> pieceFlux[3]};
    printREC["At REC 15, the new flux will take the form:"];
    printREC[newFlux];

    Table[Set[pieceFlux[jj], Map[Part[#, jj] &,
        revisedFluxes]], {jj, 1, Length[indepVars]}];
    printREC["At REC 16, the components of the flux(es) computed for the"<>
        " evolution system are"];
    Table[printREC["p[", jj, "] = ", pieceFlux[jj]], {jj, 1, Length[indepVars]}];

    compatibility = Map[Part[#, 2] &, revisedRhos];
    printREC["At REC 17, the list of compatibility conditions:"];
    printREC[compatibility];

    density = Map[Part[#, 1] &, revisedRhos];
    printREC["At REC 18, a list of densities computed under the"<>
        " evolution system:"];
    printREC[density];

    newDensity = pieceFlux[posSwapVar];
    printREC["At REC 19, the density for the PDE as originally given:"];
    printREC[newDensity];

    newFlux = Thread[List[Apply[Sequence, Table[Part[newFlux, i1],
        {i1, 1, Length[indepVars]}]]]];
    printREC["At REC 20, the flux for the PDE as originally given:"];
    printREC[newFlux];

    (* Check for terms in the reconfigured density that are divergences.*)
    checkDivergences =
        Thread[cd[newDensity, newFlux,
        Table[indepVars, {j1 ,1, Length[newDensity]}]]] /.
        cd -> checkForDivergences;
    printREC["At REC 21, after removing any divergences, the"<>
        " density/flux pairs are changed to:"];
    printREC[checkDivergences];

    newDensity = Map[Part[#, 1] &, checkDivergences];
    newFlux = Map[Part[#, 2] &, checkDivergences];
    printREC["At REC 22, the list of densities with the divergences removed:"];
    printREC[newDensity];
    printREC["At REC 23, the list of fluxes with the divergences removed:"];
    printREC[newFlux];

    (* Re normalize the recomputed density.                             *)
    newDensity =
          Map[newNormalize[#, {}, 1] &, newDensity];
    printREC["At REC 24, the density for the PDE after normalization"<>
        " with the normalization coefficient at the end of the list:"];
    printREC[newDensity];

    newFlux = Expand[Thread[tim[newFlux,
        1 /Map[Part[#, 2] &, newDensity]]] /. tim -> Times];
    printREC["At REC 25, the flux after dividing by the normalizing"<>
        " constant:"];
    printREC[newFlux];

    newDensity = Map[Part[#, 1] &, newDensity];
    printREC["At REC 26, the density after normalization:"];
    printREC[newDensity];

    newDensity =
        Thread[List[newDensity, compatibility]];
    newDensity = If[newDensity === {{{}, {}}}, {},
        DeleteCases[newDensity, {{}, {}}, {0,1}]];
    newFlux = If[newFlux === {{}}, {},
        DeleteCases[newFlux, {}, {0,1}]];
    printREC["At REC 27, the density after restructuring for the"<>
        " variable swap:"];
    printREC[newDensity];
    printREC["At REC 28, the flux after restructuring for the"<>
        " variable swap:"];
    printREC[newFlux],

  (* else *)
    newFlux = revisedFluxes;
    newDensity = revisedRhos
 ]; (* end If reverseRulesExch *)

  newFlux = If[Length[indepVars] === 1, Flatten[newFlux], newFlux];

  printREC["At REC OUT, the normalized density in original variables:"];
  printREC[newDensity];
  printREC["At REC OUT, the associated flux in original variables:"];
  printREC[newFlux];

  (* Clear all local variables not being returned.                      *)
  Clear[m1, n1, n2, n3, n4, revisedIndepVars, undoRule, noDerivRule,
      derVarLHS, derVarRHS, derivRuleLHS, derivRuleRHS, derivRule, posSwapVar,
      swapVar, indepVarSwapRule1, indepVarSwapRule2, tempt, revisedRhos,
      dummyIndVars, der, revisedFluxes, pieceFlux, density, compatibility,
      tim, checkDivergences, cd, printREC];

  Return[{newDensity, newFlux}]
] (* end Module reverseEvolutionChanges *)

(* ##########         Function: getWeightsWrapper          ########## *)

(**********************************************************************)
(* getWeightsWrapper[diffFunctionList, weightedParameters,            *)
(*                   numDepVars, userWeightRules (optional)]          *)
(* Purpose: To go from initial equations to the numerical weight      *)
(*          values of all of the variables                            *)
(* Input:   List of differential functions                            *)
(*          List of weighted parameters                               *)
(*          The number of dependent variables                         *)
(*          A list of weight rules given by the user (if any)         *)
(* Output:  A list of symbolic weight rules                           *)
(*          A list of numerical weight rules                          *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whgweiwr.m, Created 19 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpgweiwr.m                      *)
(* Major Changes Made: June 14, 2006, 6:25 by DP at CSM               *)
(* Last Modified:  4 February, 2010, 11:25 by DP at CSM               *)
(**********************************************************************)

getWeightsWrapper[diffFunctionList_List, weightedParameters_,
    numDepVars_Integer, userWeightRules_:{}] :=
Module[{weightRulesSymbolic, balanceEqns, maxWeightValue, userMode = False,
    numCalcNeeded, weightRules, freeParameters, numericalWeightRules,
    numWeightRules, limit = 10, numericCheck, printGWW},

  If[debugGetWeightsWrapper, printGWW = Print, Clear[printGWW],
      Clear[printGWW]];

  printGWW["debug code: GWW, Function: getWeightsWrapper, File: dpgweiwr.m"];

  printGWW["At GWW IN, the differential function list consists of:"];
  printGWW[diffFunctionList];
  printGWW["At GWW IN, the number of dependent variables is ", numDepVars];
  printGWW["At GWW IN, the weighted parameter list contains: ",
      weightedParameters];
  printGWW["At GWW IN, numeric weight rules given by the user consist of"];
  printGWW[userWeightRules];

  (* General rules for the weights are symbollically formed.            *)
  (* Compute weights in symbolic mode.                                  *)
  {weightRulesSymbolic, balanceEqns} = computeSymbolicWeights[diffFunctionList,
      weightedParameters, numDepVars];
  printGWW["At GWW 1, symbolic weight rules from computeWeights:"];
  printGWW[weightRulesSymbolic];

  (* Print the symbolic weights for user information.                   *)
  If[globalVerbose === All,
    printSymbolicWeightRules[Flatten[weightRulesSymbolic]];
  ]; (* end globalVerbose *)

  (* Check for zero and negative symbolic weights. If they exist, stop  *)
  (* the program.                                                       *)
  evaluateSymbolicRules[weightRulesSymbolic, balanceEqns];
  printGWW["At GWW 2, evaluateSymbolicRules has passed the symbolic rules."];

  (* Program automatically selects weights for free parameters and      *)
  (* tries to find a set that works.                                    *)
  (* Default maximum - number is arbitrary.                             *)
  maxWeightValue = 5;

  (* If the user has entered some weights in the data file,             *)
  (* replace symbolic weights with user weights.                        *)
  If[userWeightRules =!= {},
    userMode = True;
    printGWW["At GWW 3, user weight rules given are:"];
    printGWW[userWeightRules];

    (* Determine whether all weights or only part of the weights have   *)
    (* been assigned.                                                   *)
    numCalcNeeded = If[Length[userWeightRules] === Length[Union[
        Cases[weightRulesSymbolic, weight[___], Infinity]]],
      False, True];
    printGWW["At GWW 4, numerical calculations need to be carried out: ",
        numCalcNeeded];
    If[numCalcNeeded,
      weightRules = weightRulesSymbolic /. userWeightRules;
      weightRules = {Flatten[Union[weightRules, userWeightRules]]};
      printGWW["At GWW 5, symbolic weight rules with user entered weights:"];
      printGWW[weightRules],
    (* else *)
      numericalWeightRules = userWeightRules
    ], (* end If numCalcNeeded *)

  (* else if userWeightRules === {} *)
    weightRules = weightRulesSymbolic;
    numCalcNeeded = True;
    printGWW["At GWW 6, the symbolic weight rules are:"];
    printGWW[weightRules]
  ]; (* end If userWeightRules *)

  If[numCalcNeeded,
    If[userMode,
      (* Reset symbolic rules to include rules given by the user if the *)
      (* list is a partial list.                                        *)
      weightRules = resetWeightSystemForUser[weightRules, weightedParameters,
          numDepVars];
      printGWW["At GWW 7, symbolic weights adjusted for user entered"<>
          " weights:"];
      printGWW[weightRules];

      (* Find the free weight parameters.                               *)
      freeParameters = scanForFreeParameters[weightRules];
      printGWW["At GWW 8, free parameters on symbolic weights adjusted"<>
          " for user input:"];
      printGWW[freeParameters];

      (* Find numerical weights for any variables not given by the user. *)
      (* A single weight of zero is allowed.                             *)
      numericalWeightRules = findSuitableWeightSystem[weightRules,
          freeParameters, maxWeightValue, True];
      printGWW["At GWW 9, numeric weights given by findSuitableWeightSystem"<>
          " before final check:"];
      printGWW[numericalWeightRules],
    (* else !userMode *)
      (* Otherwise, we do not allow weights of zero.                    *)

      (* Find the free weight parameters.                               *)
      freeParameters = scanForFreeParameters[weightRules];
      printGWW["At GWW 10, free parameters on symbolic weights:"];
      printGWW[freeParameters];

      numericalWeightRules = findSuitableWeightSystem[weightRules,
          freeParameters, maxWeightValue, False];
      printGWW["At GWW 11, numeric weights given by findSuitableWeightSystem"<>
        " before final check:"];
      printGWW[numericalWeightRules]
    ] (* end If userMode *)
  ]; (* end If numCalcNeeded *)
  printGWW["At GWW 12, the numerical weight rules found are:"];
  printGWW[numericalWeightRules];

  (* If an empty list is found for both numerical weight rules and      *)
  (* free parameters then the system is unacceptable.                   *)
  If[numericalWeightRules === {},
    If[freeParameters === {},
      Print["The weight system is inconsistent with the symmetries of the"<>
          " PDE.  If the user has supplied weights, it may be necessary to"<>
          " make some adjustment to the list.  Otherwise, it may be"<>
          " necessary to add a weighted parameter to the PDE.  This"<>
          " can be done by moving a current parameter into the weighted"<>
          " parameter list in the data file or by placing a new parameter"<>
          " with a term or group of terms in the PDE. The introduction of"<>
          " a weighted parameter will change the weight calculations."];
      Print["All computations are being discontinued."];
      Abort[],

    (* else *)
      (* Otherwise free parameters are still available to be set,       *)
      (* so prompt the user for them.                                   *)
      numericalWeightRules = promptUserForWeights[weightRules,
           freeParameters];
    ] (* end If freeParameters *)
  ]; (* end If numericalWeightRules *)
  printGWW["At GWW 13, the numerical weight rules are:"];
  printGWW[numericalWeightRules];

  (* Test to make sure weights are good.                                *)
  numWeightRules = checkNumericWeights[numericalWeightRules,
      weightRulesSymbolic, limit];

  (* Print the numerical weights in to the user.                        *)
  printNumericWeightRules[Sort[numWeightRules]];

  (* Check that the numeric weights are indeed numeric.                 *)
  numericCheck = Map[Part[#, 2] &, numericalWeightRules];
  If[Union[Map[NumericQ, numericCheck]] =!= {True},
    Print["Weights have non-numeric values.  The program will not be"<>
        " able to compute a candidate density unless all weights have"<>
        " a numeric value."];
    Print["All computations are being discontinued."];
    Abort[]
  ]; (* end If Union[NumericQ] *)

  printGWW["At GWW OUT, the symbolic weight rules are: ", weightRulesSymbolic];
  printGWW["At GWW OUT, the numeric weight rules are: ", numWeightRules];

  (* Clear all local variables not being returned.                      *)
  Clear[maxWeightValue, userMode, numCalcNeeded, eightRules, freeParameters,
      numericalWeightRules, limit, numericCheck, printGWW];

  Return[{Flatten[weightRulesSymbolic], Flatten[numWeightRules]}]
]; (* end Module getWeightsWrapper *)

(* ##########       Function: computeSymbolicWeights       ########## *)

(**********************************************************************)
(* computeSymbolicWeights[diffFunctionList, weightedParameters,       *)
(*                        numDependentVars]                           *)
(* Purpose: To call all the helper functions for calculating the      *)
(*          weights. The output consists of replacement rules for the *)
(*          weights.  This function can be called in two modes,       *)
(*          "Symbolic"and "User."  If it is called in "Symbolic"      *)
(*          mode, all the weights are kept in symbolic form.  If      *)
(*          specifed in userWeightRules from the data file, called in *)
(*          "User" mode, the system applies the rules giving them     *)
(*          numeric values, and then solves the remaining system.     *)
(* Input:   A list of Differential Functions given for the PDE        *)
(*          A list of weighted parmeters                              *)
(*          The number of dependent variables                         *)
(* Output:  A list of symbolic replacement rules for the weights.     *)
(*          {weight[u[2]] -> weight[u[1]] - weight[d/dy]}             *)
(*          A list of weight balance equations                        *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whcomwei.m, Created 18 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpcomwei.m                      *)
(* Major Changes Made: 14 June, 2006, 6:50 by DP at CSM               *)
(* Major Changes Made: 14 December, 2006, 9:35 by DP at CSM           *)
(* Last Modified:  6 June, 2008, 7:48 by DP at home                   *)
(**********************************************************************)

computeSymbolicWeights[diffFunctionList_List, weightedParameters_List,
    numDependentVars_Integer] :=
Module[{weightList, equalList, solveOrderList, ruleList, rules, a, b, c,
    f, n1, j, printCSW},

  If[debugComputeSymbolicWeights, printCSW = Print, Clear[printCSW],
      Clear[printCSW]];

  printCSW["debug code: CSW, Function: computeSymbolicWeights,"<>
      " File: dpcomwei.m"];

  printCSW["At CSW IN the list of differential equations in evolution form:"];
  printCSW[diffFunctionList];
  printCSW["At CSW IN the weighted parameter list contains: ",
      weightedParameters];
  printCSW["At CSW IN the number of dependent variables is ",
      numDependentVars];

  Clear[d, dx, dy, dz, dt, weight];

  (* Rules to be applied to differential expressions to form weight     *)
  (* expressions in the function generateWeightOfTermsList.  These      *)
  (* rules use Dr. Hereman's method of changing the product of          *)
  (* variables into the sum of the weights of the variables.            *)
  (* e.g., Each u[i] goes to E^w(u[i] and                               *)
  (* Each D[u[i], {x, m}, {y,n}] goes to E^(n*w(Dx)+m*w(Dy)+w(u[i])     *)

  paramRule = Map[Rule[#, E^weight[#]] &, weightedParameters];
  depVarRule = u[n1_Integer][__] -> E^weight[u[n1]];
  derDepRule = Derivative[n2_, n3_:0, n4_:0, n5_][u[n1_Integer]][__] ->
      E^(weight[u[n1]] + n2*weight[d/dx] + n3*weight[d/dy] +
      n4*weight[d/dz] + n5*weight[d/dt]);
  indepRule = {x -> E^(-weight[d/dx]), y -> E^(-weight[d/dy]),
      z -> E^(-weight[d/dz]), t -> E^(-weight[d/dt])};
  printCSW["At CSW 1, Rules applied to expression:"];
  printCSW["Rules for weighted parameters: ", paramRule];
  printCSW["Rule for dependent variables: ", depVarRule];
  printCSW["Rule for dependent variables with derivatives: ", derDepRule];
  printCSW["Rule for explicit independent variables: ", indepRule];

  (* Find weight balances for individual terms in each equation.        *)
  weightList = Map[generateWeightOfTermsList[#, weightedParameters,
      Flatten[{paramRule, depVarRule, derDepRule, indepRule}], True] &,
      diffFunctionList];
  printCSW["At CSW 2, a list of expressions for calculating the weight "<>
      " of individual term is "];
  printCSW[weightList];

  (* Formulate a system of equations from the weight balances.          *)
  equalList = Flatten[Map[equateListTerms, weightList]];
  printCSW["At CSW 3, system of equations for each differential function:"];
  printCSW[equalList];

  ruleList = solveWeightBalances[equalList, weightedParameters,
      numDependentVars];
  printCSW["At CSW 4, after solveWeightBalances, list of rules for weights:"];
  printCSW[ruleList];

  (* Check for free weights.                                            *)
  freeWts = Complement[Cases[equalList, weight[_], {0, Infinity}],
      Cases[ruleList, weight[_], {0, Infinity}]];
  printCSW["At CSW 5, weights that do not appear in the symbolic weight"<>
      " list are independent of other weights:"];
  printCSW[freeWts];

  freeWts = Map[Rule[#, #] &, freeWts];
  printCSW["At CSW 6, free weights are assigned to themselves:"];
  printCSW[freeWts];

  ruleList = {Flatten[{freeWts, ruleList}]};

  printCSW["At CSW OUT, the symbolic ruleList: "];
  printCSW[ruleList];

  (* Clear all local variables not being returned.                      *)
  Clear[weightList, solveOrderList, rules, a, b, c, f, count, j, printCSW];

  Return[{ruleList, equalList}]
]; (* end Module computeSymbolicWeights *)

(* ##########      Function: evaluateSymbolicRules         ########## *)

(**********************************************************************)
(* evaluateSymbolicRules[symbolicWeightRules, balanceEqns]            *)
(* Purpose: To check for symbolic rules that require a negative or    *)
(*          zero weight rule                                          *)
(* Input:   List of symbolic weight rules                             *)
(*          A list of balance equations for weights                   *)
(* Output:  none - program stops if conditions are not met            *)
(* Created: 30 October, 2007 at CSM                                   *)
(* Code is in File:  conservationlaws/nwevsyru.m                      *)
(* Last Modified:  6 June, 2008, 7:53 by DP at home                   *)
(**********************************************************************)

evaluateSymbolicRules[symbolicWeightRules_List, balanceEqns_List]:=
    Module[{rulesList, zeroRules, rule, negativeRules, printESR},

  If[debugEvaluateSymbolicRules, printESR = Print, Clear[printESR],
      Clear[printESR]];

  printESR["debug code: ESR, Function: evaluateSymbolicRules,"<>
      " File: nwevsyru.m"];

  printESR["At ESR IN, the list of symbolic weight"<>
      "rules:"];
  printESR[symbolicWeightRules];

  rulesList = symbolicWeightRules /. Rule -> rule;
  printESR["At ESR 1, Rule has been changed to rule in the symbolic"<>
      " weight list:"];
  printESR[rulesList];

  zeroRules = Cases[rulesList, rule[weight[_], 0], {0, 2}];
  printESR["At ESR 2, symbolic weight rules calculated to be 0:"];
  printESR[zeroRules /. rule -> Rule];

  If[zeroRules =!= {},
    Print["A weight of zero has been found for one or more of the"<>
        " variables.  Since the program requires positive weights, no"<>
        " further calculations will be made.  It may be possible to"<>
        " introduce a weighted parameter to the given PDE, which will"<>
        " change the weight calculations.  If the equation already contains"<>
        " a parameter, add it to the weighted parameter list in the data"<>
        " file.  If not, choose a parameter and try placing it with"<>
        " different terms or groups of terms to see how the weight"<>
        " calculations change.  The weight balance equations are shown"<>
        " below as an aid to determine where a weighted parameter should"<>
        " be placed.  For example, the balance equation\n  weight[u[1]]"<>
        " + weight[d/dx] = 2*weight[u[1]] + weight[d/dx] requires a"<>
        " weighted parameter on the left-hand side so that all weights"<>
        " carry a positive value."];
    Print["The balance equations for the given PDE are:"];
    Print[balanceEqns];
    Print["All computations are being discontinued."];
    Abort[]
  ]; (* end If zeroList *)

  negativeRules = Cases[rulesList, rule[weight[_],
      Times[n1_, weight[_]]] /; n1 < 0, {0, 2}];
  printESR["At ESR 3, symbolic weight rules calculated to be negative:"];
  printESR[negativeRules /. rule -> Rule];

  If[negativeRules =!= {},
    Print["A negative weight has been found for one or more of the"<>
        " variables.  Since the program requires positive weights, no"<>
        " further calculations will be made.  It may be possible to"<>
        " introduce a weighted parameter to the given PDE, which will"<>
        " change the weight calculations.  If the equation already contains"<>
        " a parameter, add it to the weighted parameter list in the data"<>
        " file.  If not, choose a parameter and try placing it with"<>
        " different terms or groups of terms to see how the weight"<>
        " calculations change.  The weight balance equations are shown"<>
        " below as an aid to determine where a weighted parameter should"<>
        " be placed.  For example, the balance equation\n  weight[u[1]] +"<>
        " weight[d/dx] = 2*weight[u[1]] + weight[d/dx] requires a weighted"<>
        " parameter on the left-hand side so that all weights carry a"<>
        " positive value."];
    Print[balanceEqns];
    Print["All computations are being discontinued."];
    Abort[]
  ]; (* end If negativeList *)

  Clear[rulesList, zeroRules, rule, negativeRules, printESR]

] (* end Module evaluateSymbolicRules *)

(* ##########     Function: generateWeightOfTermsList      ########## *)

(**********************************************************************)
(* generateWeightOfTermsList[diffFunction , weightedParameters, rules *)
(*     mode]                                                          *)
(* Purpose: To change an input expression into a list of weight       *)
(*          expressions                                               *)
(* Input:   A differential Function (a single equation                *)
(*          List of weighted parameters                               *)
(*          List of rules for transforming differential expressions   *)
(*              into weight equations                                 *)
(*          The mode, which controls calls on the stripper            *)
(* Output:  A list of weight expressions for each term of the input   *)
(*          expression, e.g.,                                         *)
(*            {weight[u[1]] + weight[d/dt], 2*weight[u[1]] +          *)
(*            weight[d/dx] + weight[d/dz], weight[u[1]] +             *)
(*            3*weight[d/dx]}                                         *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whgewetl.m, Created 13 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpgewetl.m                      *)
(* Major Changes Made: 25 May, 2006, 10:40 by DP at CSM               *)
(* Last Modified: 28 April, 2008, 12:59 by DP at CSM                  *)
(**********************************************************************)

generateWeightOfTermsList[diffFunction_, weightedParameters_List,
    rules_, mode_] :=
Module[{strippedList, weightList, printGWOTL},

  If[debugGenerateWeightOfTermsList, printGWOTL = Print, Clear[printGWOTL],
      Clear[printGWOTL]];

  printGWOTL["debug code: GWOTL, Function: generateWeightOfTermsList,"<>
    " File: dpgewetl.m"];

  printGWOTL["At GWOTL IN, the given differential function:"];
  printGWOTL[diffFunction];
  printGWOTL["At GWOTL IN, a list of weighted parameters:"];
  printGWOTL[weightedParameters];
  printGWOTL["At GWOTL IN, the rules for translating differential"<>
      " expressions to weight expressions are:"];
  printGWOTL[rules];

  (* Return so the stripper does not get messed up                      *)
  If[diffFunction === 0, Return[{}]];

  (* Strip off the coefficients from the input expression.              *)
  (* The terms are returned in a list.                                  *)
  (* Stripper is not needed when function is run to separate rho terms  *)
  (* into buckets.                                                      *)
  If[mode,
    strippedList = stripper[diffFunction, weightedParameters],
  (* else *)
    strippedList = diffFunction;
  ]; (* end If mode *)

  printGWOTL["At GWOTL 1, a list of differential function terms after",
       " going through the stripper:"];
  printGWOTL[strippedList];

  (* Apply the input rules to the input expression.                     *)
  weightList = strippedList /. rules;
  printGWOTL["At GWOTL 2, a list of differential function terms after",
      " applying the rules:"];
  printGWOTL[weightList];

  (* Remove Exponentials from the list of expressions.                  *)
  (* Log will remove all of the exponentials that we used               *)
  (* PowerExpand forces all of the exponentials to cancel with the logs *)
  weightList = PowerExpand[Log[E, weightList]];
  printGWOTL["At GWOTL 3, a list after log was taken:"];
  printGWOTL[weightList];

  (* Remove duplicates.                                                 *)
  weightList = Union[weightList];
  printGWOTL["At GWOTL 4, a list after Union is applied:"];
  printGWOTL[weightList];

  (* Remove any cases that are zeros                                    *)
  weightList = DeleteCases[weightList, 0];
  printGWOTL["At GWOTL 5, a list after zeros are removed:"];
  printGWOTL[weightList];

  (* Clear all local variables not being returned.                      *)
  Clear[strippedList, printGWOTL];

  Return[weightList]
]; (* end Module generateWeightOfTermsList *)

(* ##########           Function: equateListTerms          ########## *)

(**********************************************************************)
(* equateListTerms[inputList]                                         *)
(* Purpose: To form a set of equations given several equivalent       *)
(*          weight expressions.                                       *)
(* Input:   A list of weight terms                                    *)
(*          For Example:                                              *)
(*          ------------                                              *)
(*          {weight[u[1]]+weight[d/dy],                               *)
(*          2*weight[d/dx]+weight[u[2]],                              *)
(*          weight[d/dt]+weight[u[3]]}                                *)
(* Output:  List of a system of weight equalities                     *)
(*          For Example:                                              *)
(*          ------------                                              *)
(*          {weight[u[1]]+weight[d/dy] == 2*weight[d/dx]+weight[u[2]],*)
(*          weight[u[1]]+weight[d/dy] == weight[d/dt]+weight[u[3]]}   *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                wheqlite.m, Created 13 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpeqlite.m                      *)
(* Major Changes Made: 25 May, 2006, 12:10 by DP at CSM               *)
(* Last Modified:  28 April, 2008, 13:10 by DP at CSM                 *)
(**********************************************************************)

equateListTerms[inputList_List] :=
Module[{inputListRevised, equalList, firstExpn, printELT},

  If[debugEquateTerms, printELT = Print, Clear[printELT], Clear[printELT]];

  printELT["debug code: ELT, Function: equateListTerms, File: dpeqlite.m"];

  printELT["At ELT IN, the list of terms to equate:"];
  printELT[inputList];

  (* If there is only one item in the input list, there is nothing to   *)
  (* equate.                                                            *)
  If[Length[inputList] === 1, Return[{}]];

  (* Equate the first term to each of the other terms in the list.      *)
  firstExpn = Part[inputList , 1];
  inputListRevised = Delete[inputList, {{1}}];
  equalList = Map[Equal[#, firstExpn] &, inputListRevised];
  printELT["At ELT 1, equalList:"];
  printELT[equalList];

  printELT["At ELT OUT, the list of equal terms after equateListTerms is:"];
  printELT[equalList];

  (* Clear all local variables not being returned.                      *)
  Clear[inputListRevised, firstExpn, printELT];

  (* Return list of equalities                                          *)
  Return[equalList]
]; (* end Module equateListTerms *)

(* ##########         Function: solveWeightBalances        ########## *)

(**********************************************************************)
(* solveWeightBalances[balanceSystem, weightedParameters,numDeptVars] *)
(* Purpose: To determine weights in symbolic or numeric form,         *)
(*          depending on input, given a system of weight equations    *)
(*          formed from scaling symmetries.                           *)
(* Input:   A list of expressions from the data file                  *)
(*          A list of weighted parmeters                              *)
(*          The number of dependent variables                         *)
(* Output:  list of replacement rules for the weights.                *)
(*          {weight[u[2]]->weight[u[1]]-weight[d/dy]}                 *)
(* Created: 18 December, 2006 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwsoweba.m                      *)
(* Last Modified:  28 April, 2006, 13:16 by DP at CSM                 *)
(**********************************************************************)

solveWeightBalances[balanceSystem_List, weightedParameters_List,
     numDeptVars_Integer] :=
Module[{uList, freePars, solveOrderList, ruleList, j1, printSWB},

  If[debugSolveWeightBalances, printSWB = Print, Clear[printSWB],
      Clear[printSWB]];

  printSWB["debug code: SWB, Function: solveWeightBalances,"<>
      " File: nwsoweba.m"];

  printSWB["At SWB IN, the given weight system:"];
  printSWB[balanceSystem];
  printSWB["At SWB IN, the weighted parameter list contains: ",
      weightedParameters];
  printSWB["At SWB IN, the number of dependent variables is ", numDeptVars];

  (* Set up list of dependent variables and weighted parameters.        *)
  uList = Table[weight[u[j1]], {j1, 1, numDeptVars}];
  freePars = Map[weight, weightedParameters];

  (* Make a solve order list which defines which variables should be    *)
  (* solved for first.                                                  *)
  solveOrderList = Flatten[{weight[d/dt], uList , freePars, weight[d/dz],
      weight[d/dy], weight[d/dx]}];
  printSWB["At SWB 1, solveOrderList is:"];
  printSWB[solveOrderList];

  (* Create a list of symbolic rules for the weights of all parts of    *)
  (* differential function.                                             *)
  ruleList = Solve[Flatten[balanceSystem], solveOrderList];
  printSWB["At SWB 2, the ruleList is: "];
  printSWB[ruleList];

  (* Clear all local variables not being returned.                      *)
  Clear[uList, freePars, solveOrderList, j1, printSWB];

  Return[ruleList]
]; (* end Module solveWeightBalances *)

(* ##########      Function: resetWeightSystemForUser      ########## *)

(**********************************************************************)
(* resetWeightSystemForUser[userAlteredWeightRules, weightedParams,   *)
(*                         numDepenVars]                              *)
(* Purpose: To rewrite the symbolic weight rules after applying       *)
(*          weight rules given by the user.                           *)
(* Input:   System of weight rules altered by user input weights      *)
(*          List of free parameters                                   *)
(*          Number of dependent variables, weighted parameters        *)
(* Output:  A revised list of rules for weights                       *)
(* Created: 18 December, 2006 by DP  at CSM                           *)
(* Code is in File:  conservationlaws/nwrwesfu.m                      *)
(* Last Modified: 28 April, 2008, 13:26 by DP  at CSM                 *)
(**********************************************************************)

resetWeightSystemForUser[userAlteredWeightRules_List, weightedParams_List,
    numDepenVars_Integer]:=
Module[{alteredSystem, newSystem, printRWSU},

  If[debugResetWeightSystemForUser, printRWSU = Print, Clear[printRWSU],
      Clear[printRWSU]];

  printRWSU["debug code: RWSU, Function: resetWeightSystemForUser,"<>
      " File: nwrwesfu.m"];

  printRWSU["At RWSU IN, solving for freeParameters in the system:"];
  printRWSU[userAlteredWeightRules];
  printRWSU["At RWSU IN, the number of dependent variables is ", numDepenVars];
  printRWSU["At RWSU IN, a list of weighted parameters contains:"];
  printRWSU[weightedParams];

  (* Set up and solve a new system with user supplied rules inserted. *)
  alteredSystem = Flatten[userAlteredWeightRules /. Rule -> Equal];
  printRWSU["At RWSU 1, the list of weight rules made into equations:"];
  printRWSU[alteredSystem];

  (* Check for user given weights that are incompatible with the      *)
  (* symbolic weights.                                                *)
  If[MemberQ[alteredSystem, False],
    Print["User supplied weights are not compatible with the symbolic"<>
        " weight rules calculated.  Try supplying a different set of"<>
        " weights."];
    Abort[]
    ]; (* end If MemberQ *)

  (* Solve the system using solveWeightBalances which has the correct *)
  (* order for the variables.                                         *)
  newSystem = solveWeightBalances[alteredSystem, weightedParams,
      numDepenVars];
  printRWSU["At RWSU 2, after solving the system according to solveList:"];
  printRWSU[newSystem];

  (* Clear all local variables not being returned.                      *)
  Clear[alteredSystem, printRWSU];

  Return[newSystem]
]; (* end Module resetWeightSystemForUser *)

(* ##########       Function: scanForFreeParameters        ########## *)

(**********************************************************************)
(* scanForFreeParameters[weightRulesInput]                            *)
(* Purpose: To extract the free parameters from the right hand side   *)
(*          of a system of weight rules                               *)
(* Input:   A list of weight rules                                    *)
(*          For Example:                                              *)
(*          ------------                                              *)
(*          {weight[u[1]] -> weight[d/dx] + weight[d/dy],             *)
(*          weight[u[2]] -> 2*weight[d/dx]}                           *)
(* Output:  A list of free parameters                                 *)
(*          For Example:                                              *)
(*          ------------                                              *)
(*          {weight[d/dx], weight[d/dy]}                              *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whscfpar.m, Created 14 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpscfpar.m                      *)
(* Major Changes Made: 26 May, 2006, 8:30 by DP at CSM                *)
(* Last Modified:  28 April, 2008, 15:13 by DP at home                *)
(**********************************************************************)

scanForFreeParameters[weightRulesInput_List] :=
Module[{weightRules, freeParameters, printSFFP},

  If[debugScanForFreeParameters, printSFFP = Print, Clear[printSFFP],
      Clear[printSFFP]];

  printSFFP["debug code: SFFP, Function: scanForFreeParameters,"<>
    " File: dpscfpar.m"];

  printSFFP["At SFFP IN, the list of weight rules:"];
  printSFFP[weightRulesInput];

  (* Change each rule into a list.                                      *)
  weightRules = Flatten[weightRulesInput /. Rule -> List, 1];
  printSFFP["At SFFP 1, the weightRules list after changing Rule to List:"];
  printSFFP[weightRules];

  (* Extract part two of each list, which contains the right hand side  *)
  (* of each rule.                                                      *)
  freeParameters = Map[Part[#, 2] &, weightRules];
  printSFFP["At SFFP 2, the initial list of free parameters is"];
  printSFFP[freeParameters];

  (* Turn everything into a list.                                       *)
  freeParameters = Flatten[freeParameters /. Plus -> List];
  printSFFP["At SFFP 3, the free parameters list after removing Plus:"];
  printSFFP[freeParameters];

  (* Pick out terms that have weight[] only.                            *)
  freeParameters = Cases[freeParameters, weight[___], Infinity];
  printSFFP["At SFFP 4, the free parameters list after removing all non"<>
      " weight terms:"];
  printSFFP[freeParameters];

  (* Remove duplicates.                                                 *)
  freeParameters = Union[freeParameters];
  printSFFP["At SFFP 5, the free parameters list after removing duplicates:"];
  printSFFP[freeParameters];

  (* Clear all local variables not being returned.                      *)
  Clear[weightRules, printSFFP];

  Return[freeParameters]
]; (* end Module scanForFreeParameters *)

(* ##########      Function: findSuitableWeightSystem      ########## *)

(**********************************************************************)
(* findSuitableWeightSystem[systemOfWeightRules, freeParameters,      *)
(*                          maximumWeight, acceptZeros]               *)
(* Purpose: To provide a set of possible weights in rule form         *)
(* Input:   System of weight rules,                                   *)
(*          List of free parameters,                                  *)
(*          Maximum weight allowed                                    *)
(*          Accept zeros symbol(true or false)                        *)
(* Output:  A list of rules for weights                               *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whfiwesy.m, Created 17 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpfiwesy.m                      *)
(* Major Changes Made: 26 May, 2006, 9:25 by DP at CSM                *)
(* Major Changes Made: 15 December, 2006, 14:50 by DP at CSM          *)
(* Last Modified:  28 April, 2008, 15:39 by DP at CSM                 *)
(**********************************************************************)

findSuitableWeightSystem[systemOfWeightRules_List, freeParameters_List,
    maximumWeight_Integer, acceptZeros_Symbol]:=
Module[{numFreeParams, i1, freeParamValueTrial, weightListResult, ttt = 1,
    trialResult = True, printFSWS},

  If[debugFindSuitableWeightSystem, printFSWS = Print, Clear[printFSWS],
      Clear[printFSWS]];

  printFSWS["debug code: FSWS, Function: findSuitableWeightSystem,"<>
      " File: dpfiwesy.m"];

  printFSWS["At FSWS IN, the system of weight rules given:"];
  printFSWS[systemOfWeightRules];
  printFSWS["At FSWS IN, the free independent parameters in the system:"];
  printFSWS[freeParameters];
  printFSWS["At FSWS IN, the possibility of having zero weights is ",
      acceptZeros];
  printFSWS["At FSWS IN, the maximum weight that can be calculated is: ",
      maximumWeight];

  (* Find the number of free parameters for our loops below.            *)
  numFreeParams = Length[freeParameters];

  (* Set the first trial.                                               *)
  freeParamValueTrial = Table[1, {i1, 1, numFreeParams}];
  printFSWS["At FSWS 1, for trial 1, freeParamValueTrial = "<>
      "", freeParamValueTrial];

  (* Loop over all of the possible trial weight lists.                  *)
  While[trialResult,
    (* Pass the current trial to evaluateWeightsForFreeParameters to    *)
    (* test whether it produces a usable system of numerical weights.   *)
    weightListResult = evaluateWeightsForFreeParameters[systemOfWeightRules,
        freeParameters, freeParamValueTrial, acceptZeros];
    printFSWS["At FSWS 2, the result given by"<>
        " evaluateWeightsForFreeParameters is " , weightListResult, "."];

    (* Either end the loop, or try the next set of weights.             *)
    If[weightListResult =!= {"tryagain"},
      weightListResult = DeleteCases[weightListResult,
          Rule[_Integer, _Integer], {0,2}];
      printFSWS["At FSWS OUT, the rule list is determined to be:"];
      printFSWS[weightListResult];

      Clear[numFreeParams, i1, freeParamValueTrial, ttt, trialResult,
          printFSWS];
      Return[weightListResult]
    ]; (* end If weightListResult *)

    (* Check to see if maximum possible trials have taken place.        *)
    If[ttt >= maximumWeight^numFreeParams,
      trialResult = False;
      Print["No suitable weights were found for this system up to weight "<>
          "", maximumWeight],
    (* else *)
      (* Increment the trial list values before rerunning the loop.     *)
      freeParamValueTrial =
          Table[Mod[IntegerPart[ttt/maximumWeight^(i-1)], maximumWeight] + 1,
          {i, 1, numFreeParams}];
      printFSWS["At FSWS 3, for trial ", ttt + 1, ", freeParamValueTrial = "<>
          "", freeParamValueTrial]
    ]; (* end If ttt *)
    ttt++
  ];(* end while *)

  (* If no acceptable values are found from any of the input trial      *)
  (*  weights then just return an empty list.                           *)
  printFSWS["At FSWS 4, this list is unacceptable."];

  printFSWS["At FSWS OUT, an unacceptable rule list has been determined,"<>
      " so {} is being returned."];

  Clear[numFreeParams, i1, freeParamValueTrial, ttt, trialResult, printFSWS];

  Return[{}]
]; (* end Module findSuitableWeightSystem *)

(* ##########  Function: evaluateWeightsForFreeParameters  ########## *)

(**********************************************************************)
(* evaluateWeightsForFreeParameters[systemOfWeightRules,              *)
(*    freeParameters, trialWeights, acceptZeros]                      *)
(* Purpose: To evaluate a set of values given for the free parameters *)
(*          that remain after caluclation of the symbolic weights.    *)
(*          The set of values passes the evaluation when it produces  *)
(*          positive integer values for all weights.                  *)
(* Input:   System of rules,                                          *)
(*          Free parameters,                                          *)
(*          A set of possible number values for free parameters,      *)
(*          Accept zeros symbol(true or false)                        *)
(* Output:  A list of rules for weights                               *)
(* Created: 15 December 2006, 8:30 by DP at CSM                       *)
(* Code is in File:  conservationlaws/nwevwffp.m                      *)
(* Last Modified:  28 April, 2008, 15:54 by DP at CSM                 *)
(**********************************************************************)

evaluateWeightsForFreeParameters[systemOfWeightRules_List,
    freeParameters_List, trialWeights_List, acceptZeros_Symbol]:=
Module[{ruleList, n1, dependentRuleList, tempList, tempNegativeList,
    tempZeroList, printEWFF},

  If[debugEvaluateWeightsParameters, printEWFF = Print, Clear[printEWFF],
      Clear[printEWFF]];

  printEWFF["debug code: EWFF, Function: evaluateWeightsForFreeParameters,"<>
      " File: nwevwffp.m"];

  printEWFF["At EWFF IN, solving for ", freeParameters,  "in the system:"];
  printEWFF[systemOfWeightRules];
  printEWFF["At EWFF IN the trial weights being used are; ", trialWeights];
  printEWFF["At EWFF IN the possibility of having zero weights is ",
      acceptZeros];

  (* Make a rule list by creating a rule with each of the free          *)
  (* parameters going to a member of the trial weights list.            *)
  ruleList = Thread[Rule[freeParameters, trialWeights]];
  printEWFF["At EWFF 1, the rule list created from each of the"<>
            " free weights:"];
  printEWFF[ruleList];

  (* The list of rules for variables that are dependent variables is    *)
  (* then created by applying the newly created rules to the system of  *)
  (* weight rules.                                                      *)
  dependentRuleList = systemOfWeightRules /. ruleList;
  printEWFF["At EWFF 2, the rule list for dependent weights:"];
  printEWFF[dependentRuleList];

  (* The overall rule system can is created from taking the union of    *)
  (* the dependent rules list and the independent rules.                *)
  ruleList = Flatten[Union[ruleList, dependentRuleList]];
  printEWFF["At EWFF 3, the system list of rules over the union of"<>
      " dependent and independent weights:"];
  printEWFF[ruleList];

  (* Check all of the weights (the numbers that are the second          *)
  (* argument to each Rule function) Put these in to tempList and       *)
  (* check if the weights are acceptable.                               *)
  tempList = Map[Part[#, 2] &, ruleList];
  printEWFF["At EWFF 4, temporary list of weights to be checked"<>
      " for acceptablility:"];
  printEWFF[tempList];

  (* To be able to check whether the weight rules are acceptable        *)
  (* we have know whether or not one zero is allowed.                   *)
  (* More than one zero weight would result in having to solve PDEs.    *)
  If[acceptZeros, (* You are allowed to accept one zero *)

    (* To test this make two lists:                                     *)
    (* tempNegativeList is a list of the negative numbers.              *)
    (*    It shouldn't contain any values to pass the test.             *)
    (* tempZeroList - is a list of zero values.                         *)
    (*    It should contain at most one value to pass.                  *)
    tempNegativeList = Cases[tempList, n1_ /; n1 < 0];
    tempZeroList = Cases[tempList, 0];
    printEWFF["At EWFF 5, a list of negative numbers:"];
    printEWFF[tempNegativeList];
    printEWFF["At EWFF 6, a list of zeros:"];
    printEWFF[tempZeroList];

    (* Make sure that the number of negatives is 0 and that the number  *)
    (* of zeroes is <= to 1. If this is true, then return the values.   *)
    If[tempNegativeList === {} &&
        (tempZeroList === {} || tempZeroList === {0}),
      printEWFF["At EWFF OUT, a rule list has been found:"];
      printEWFF[ruleList];

      Clear[n1, dependentRuleList, tempList, tempNegativeList,tempZeroList,
          printEWFF];
      Return[ruleList]
    ], (* end If tempNegativeList *)
  (* else, if you are forbidden to accept one zero *)

    (* To test, make a list of zero OR negative values.                 *)
    tempList = Cases[tempList, n1_ /; n1 <= 0];
    printEWFF["At EWFF 7, a list of negative numbers and zeros:"];
    printEWFF[tempList];

    (* Test to make sure that there aren't any zero or negative         *)
    (* numbers.                                                         *)
    If[tempList === {},
      (* If temp list is empty, return the values.                      *)
      printEWFF["At EWFF OUT, a rule list has been found:"];
      printEWFF[ruleList];

      Clear[n1, dependentRuleList, tempList, tempNegativeList,tempZeroList,
          printEWFF];
      Return[ruleList]
    ] (* end if tempList *)
  ]; (* end if acceptZeros *)

  (* The set of values did not pass the test and no return has occurred.*)
  printEWFF["At EWFF OUT, no rule list has been determined."];

  (* Clear all local variables not being returned.                      *)
  Clear[n1, dependentRuleList, tempList, tempNegativeList,tempZeroList,
      printEWFF, ruleList];

  Return[{"tryagain"}]
]; (* end Module evaluateWeightsForFreeParameters *)

(* ##########       Function: promptUserForWeights         ########## *)

(**********************************************************************)
(* promptUserForWeights[systemOfWeightRules, freeParameters]          *)
(* Purpose: To get weights from the user                              *)
(* Input:   System of weight rules                                    *)
(*          A list of free parameters                                 *)
(* Output:  Numerical values for weight rules                         *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whpuswei.m, Created 19 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dppuswei.m                      *)
(* Major Changes Made: 26 May, 2006, 8:00 by DP at CSM                *)
(* Last Modified:  28 April, 2008, 16:12 by DP at CSM                 *)
(**********************************************************************)

promptUserForWeights[systemOfWeightRules_List, freeParameters_List] :=
Module[{numFreeParams, select, userRules, intelligibleAnswer, newWtSystem,
    newMaxValue = 5, printPUFW},

  If[debugPromptUserForWeights, printPUFW = Print, Clear[printPUFW],
      Clear[printPUFW]];

  printPUFW["debug code: PUFW, Function: promptUserForWeights,"<>
      " File: dppuswei.m"];

  printPUFW["At PUFW IN,  free parameters ", freeParameters, "are in the"<>
      " system:"];
  printPUFW[systemOfWeightRules];

  (* Find the number of free parameters.                                *)
  numFreeParams = Length[freeParameters];

  Print["There ", If[numFreeParams === 1, "is ", "are "], numFreeParams, ""<>
      " free weight", If[numFreeParams > 1, "s", ""], " for this system,"<>
      " for which the program has been unable to determine values for."<>
      "  The program is limited to assigning integers from 1 to 5 to"<>
      " weights.  In order to work with weights that are fractions or"<>
      " weights that are greater that 5, it is necessary that the user"<>
      " directly input suggested values for these weights or change the"<>
      " maximum value allowed."];

  (* Choose whether to increase the maximum value or self enter values  *)
  (* for the free parameters.                                           *)
  select = promptMenu;

  (* If user wishes to enter values.                                    *)
  If[select === "inputValues",

  (* Run this loop until the new system is returned or the user aborts. *)
    While[True,

      (* Clear and reset the userRules variable.                        *)
      Clear[userRules];
      userRules = {};
      intelligibleAnswer = 0;

      (* Have the user input free parameters.                          *)
      Print["There ", If[numFreeParams === 1, "is ", "are "], numFreeParams, ""<>
          " free weight", If[numFreeParams > 1, "s", ""], " for this"<>
          " system, of weight", If[numFreeParams > 1, "s", ""], " as"<>
          " shown in this list:"];
      Print[freeParameters];
      Print["Enter values for these free parameters in a list in the order"<>
          " that they are shown in the list."];
      userRules = Input["Enter values for the free parameters as\n
          {free parameter entry, free parameter entry, ...}.  Enter qq"<>
           " if you wish to quit"];
      printPUFW["At PUFW 1, user rules that have been given are:"];
      printPUFW[userRules];
      If[userRules === qq || userRules === {qq}, Abort[]];
      While[intelligibleAnswer === 0,
        If[Length[userRules] =!= Length[freeParameters] ||
            Head[userRules] =!= List,
          Print["Please re-enter your list.  The list must have "<>
              "", Length[freeParameters], " item(s) contained in braces, {},"<>
              " or qq if you wish to quit."];
          userRules = Input["Weight values in the form {a, b, c}?"];
          printPUFW["At PUFW 1, user rules that have been given are:"];
          printPUFW[userRules];
          If[userRules === qq || userRules === {qq}, Abort[]],
          intelligibleAnswer = 1;
        ]; (* end If Length *)
      ]; (* end While *)

      (* Evaluate user entered values to see if a system of numerical   *)
      (* weights can be established.                                    *)
      newWtSystem = evaluateWeightsForFreeParameters[systemOfWeightRules,
          freeParameters, userRules, False];
      printPUFW["At PUFW 2, results from evaluating user input:"];
      printPUFW[newWtSystem];

      (* If there is a new system, return it.                           *)
      If[newWtSystem =!= {"tryagain"},
        printPUFW["At PUFW OUT, a new weight system has been determined:"];
        printPUFW[newWtSystem];

        Clear[numFreeParams, select, userRules, intelligibleAnswer, newMaxValue,
            printPUFW];
        Return[newWtSystem]
      ]; (* end If newWtSystem *)

      (* Otherwise, report that the system is invalid and continue the  *)
      (* loop.                                                          *)
      Print["The entered weights produces a system with more than one",
          " zero or at least one negative number."];
      Print["Try other values or type qq to exit."];
    ]; (* end While True *)
  ]; (* end If select === inputValues *)

  (* If user wisher to change maximum weight.                           *)
  If[select === "maxWtChange",
    Print["This option will test the free weights for all possible integer"<>
        " combinations from 1 to the number you choose for your maximum"<>
        " value.  If there are several free weights, a large maximum value"<>
        " could cause a significant increase in computation time."];
    Print["Choose an integer value for your maximum value greater than 5."];

    newMaxValue = Input["Choose a new maximum value.  Enter qq if you"<>
        " wish to quit"];
    printPUFW["At PUFW 3, the new maximum value that has been given is:"];
    printPUFW[newMaxValue];
    If[newMaxValue === "qq" || userRules === {"qq"}, Abort[]];
    While[Head[newMaxValue] =!= Integer,
      Print["Please re-enter your value.  It must be a positive"<>
          " integer."];
      newMaxValue = Input["Enter a positive integer for your maximum value."<>
          "  Enter qq if you wish to quit"];
      printPUFW["At PUFW 3, the new maximum value that has been given is:"];
      printPUFW[newMaxValue];
      If[newMaxValue === "qq" || userRules === {"qq"}, Abort[]];
    ]; (* end While Head *)

    printIWWF[StyleForm["Entering findSuitableWeightSystem",
        FontSlant->Italic]];
    newWtSystem = findSuitableWeightSystem[systemOfWeightRules,
        freeParameters, newMaxValue, False];
    printIWWF[StyleForm["Exiting findSuitableWeightSystem",
        FontSlant->Italic]];
    printPUFW["At PUFW 4, the system returned by findSuitableWeightSystem:"];
    printPUFW[newMaxValue];

    (* If there is a new system, return it.                             *)
    If[newWtSystem =!= {},
      printPUFW["At PUFW OUT, a new weight system has been determined:"];
      printPUFW[newWtSystem];

      Clear[numFreeParams, select, userRules, intelligibleAnswer, newMaxValue,
          printPUFW];
      Return[newWtSystem];
    ]; (* end If newWtSystem *)

    Print["No system of numerical weights that will satisify the conditions"<>
        " for continuing this program has been found."];
  ]; (* end If select === maxWtChange *)

  Print["Nothing has worked to this point.  Try changing userWeightRules in"<>
      " your input file before running the program again."];

  Clear[numFreeParams, select, userRules, intelligibleAnswer, newMaxValue,
      newWtSystem, printPUFW];

  Abort[];
] (* end Module promptUserForWeights *)

(* ##########              Function: promptMenu            ########## *)

(**********************************************************************)
(* promptMenu (no arguments)                                          *)
(* Purpose: A menu to choose which manner is needed to input          *)
(*          information to try to determine free weights.             *)
(* Input:   None.                                                     *)
(* Output:  The menu is loaded.                                       *)
(* Created: 18 December, 2006 by DP  at CSM                           *)
(* Code is in File:  conservationlaws/nwprmenu.m                      *)
(* Last Modified:  18 December, 2006, 12:55 by DP at CSM              *)
(**********************************************************************)

promptMenu := Module[
  {choice, wish},
  Print[" "];
  Print["  *** MENU INTERFACE *** "];
  Print["-------------------------------------------"];
  Print["  1) Enter values for the free weights"];
  Print["  2) Change the maximum weight the program is allowed to use"];
  Print["     (This could increase running time dramatically!)"];
  Print["  qq) Exit the Program "];
  Print["-------------------------------------------"];

  choice = Input["ENTER YOUR CHOICE: "];

  wish = Switch[choice,
   1, "inputValues",
   2, "maxWtChange",
  qq, Abort[],
   _, Print["All computations are being discontinued."]; Abort[];
  ]; (* end switch *)

  Return[wish]
]; (* end Module promptMenu *)

(* ##########        Function: checkNumericWeights         ########## *)

(**********************************************************************)
(* checkNumericWeights[numWeightsIn, symbolicWeightsIn, limit]        *)
(* Purpose: To make sure the numeric weights that were found are      *)
(*          correct                                                   *)
(* Input:   A list of numeric weight rules                            *)
(*          A list of symbolic weight rules                           *)
(*          An upper limit to the number of checks                    *)
(* Output:  A list of numeric weight rules that are either the same   *)
(*          as the input, or a list of new numeric input from the     *)
(*          user if the input is wrong                                *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whchnumw.m, Created 3 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpchnumw.m                      *)
(* Major Changes Made: 26 May, 2006, 13:35 by DP at CSM               *)
(* Last Modified:  28 April, 2008, 16:27 by DP at CSM                 *)
(**********************************************************************)

checkNumericWeights[numWeightsIn_, symbolicWeightsIn_, limit_] :=
Module[{symbolicWeights, weightsAreBad, freeP, newWeights, printCNW},

  If[debugCheckNumericWeights, printCNW = Print, Clear[printCNW],
      Clear[printCNW]];

  printCNW["debug code: CNW, Function: checkNumericWeights, File: dpchnumw.m"];

  printCNW["At CNW IN, the list of numeric weights given:"];
  printCNW[numWeightsIn];
  printCNW["At CNW IN, the symbolic system to be used to check the numeric"<>
      " weights:"];
  printCNW[symbolicWeightsIn];
  printCNW["At CNW IN, the limit is ", limit];

  (* Make symbolic weight rules into lists for easier comparison.       *)
  symbolicWeights = symbolicWeightsIn /. Rule -> List;
  printCNW["At CNW 1, symbolicWeights after Rule has been replaced by List:"];
  printCNW[symbolicWeights];
  (* Replace weights with their numerical values in the symbolic        *)
  (* equations.                                                         *)
  symbolicWeights = Flatten[symbolicWeights /. numWeightsIn, 1];
  printCNW["At CNW 2, symbolicWeights replaced by numerical values:"];
  printCNW[symbolicWeights];

  weightsAreBad = Apply[SameQ, symbolicWeights, 1];
  printCNW["If all entries are True, symbolic weights pass the test."];
  printCNW["At CNW 3, weightsAreBad list:"];
  printCNW[weightsAreBad];

  (* If numeric weights don't match within each eqn, prompt user.       *)
  If[MemberQ[weightsAreBad, False],
    printCNW["Prompt user now."];
    printNumericWeightRules[numWeightsIn];
    Print["The numeric weights calculated as shown above are invalid."];
    printSymbolicWeightRules[symbolicWeightsIn];
    Print["Please enter new values"];

    freeP = scanForFreeParameters[symbolicWeightsIn];

    newWeights = promptUserForWeights[ symbolicWeightsIn, freeP ];
    printCNW["At CNW 4, new numeric weights to check:"];
    printCNW[newWeights];

    checkNumericWeights[newWeights, symbolicWeightsIn, limit-1],

  (* else *)
    newWeights = numWeightsIn
  ]; (* end If MemberQ *)

  printCNW["At CNW 5, numeric weight rules after finishing loop:"];
  printCNW[newWeights];

  (* Clear all local variables not being returned.                      *)
  Clear[symbolicWeights, weightsAreBad, freeP, printCNW];

  Return[newWeights]
]; (* end Module checkNumericWeights *)

(* ##########          Function: buildRhoWrapper           ########## *)

(**********************************************************************)
(* buildRhoWrapper[symbolicWeightListIn, numericalWeightListIn,       *)
(*     numDepVars, weightedParameters, indepVarList, rhoRank,         *)
(*     explicitInd]                                                   *)
(* Purpose: To call all the functions that work to build Rho.         *)
(* Input:   Numerical weight list                                     *)
(*          Symbolic weight list                                      *)
(*          Number of dependent variables                             *)
(*          A list of independent variables                           *)
(*          A list of weighted parameters                             *)
(*          The rank of density given in the data file (optional)     *)
(*          The highest degree allowed for explicit independent       *)
(*              variables as given in the data file (optional)        *)
(* Output:  The rank given for the density                            *)
(*          A list of candidate densities with undetermined           *)
(*              coefficients                                          *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whbrhowr.m, Created 26 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpbrhowr.m                      *)
(* Major Changes Made:  1 June, 2006, 10:30 by DP at CSM              *)
(* Last Modified:  17 August, 2008, 9:34 by DP at CSM                 *)
(**********************************************************************)

buildRhoWrapper[symbolicWeightListIn_, numericalWeightListIn_, numDepVars_,
    weightedParameters_, indepVarList_, rhoRank_, explicitInd_] :=
Module[{indepans = 0, indepVarListWitht = Flatten[{indepVarList, t}], printBRW,
    userDesignatedRank, originalRank, numericalWeightList, rhoInBuckets},

  If[debugBuildRhoWrapper, printBRW = Print, Clear[printBRW], Clear[printBRW]];

  printBRW["At BRW IN, a list of symbolic weights:"];
  printBRW[symbolicWeightListIn];
  printBRW["At BRW IN, a list of numeric weights: "];
  printBRW[numericalWeightListIn];
  printBRW["At BRW IN, the number of dependent variables is ", numDepVars];
  printBRW["At BRW IN, the independent variable list is ", indepVarList];
  printBRW["At BRW IN, the weighted parameter list contains: "];
  printBRW[weightedParameters];
  printBRW["At BRW IN, the density given from the file is: ", rhoRank];
  printBRW["At BRW IN, the highest degree for any explicit independent"<>
      " variables is ", explicitInd];

  (* A prompt is made to establish whether the user wishes to find      *)
  (* densities with explicit independent variables in the terms.        *)
  If[explicitInd === Null,
    Print["An input window should appear asking the user to whether to"<>
        " include explicit independent variables in the calculations."<>
        "  If no input box appears, it may be hiding behind the current"<>
        " window.  Lower or reduce the window to find it."];
    While[indepans =!= 1 && indepans =!= 2,
      indepans = Input["If the user wishes to calculate densities without"<>
          " explicit dependence on "<> ToString[indepVarListWitht] <>
          ","<>" enter 1.  If the user wishes to calculate densities"<>
         " explicitly dependent on "<>ToString[indepVarListWitht]<>
	  ","<>" enter 2."];
    ], (* end While *)
    (* else *)
    If[explicitInd === 0, indepans = 1, indepans = 2]
  ]; (* end If explicitInd *)

  (* The user can specify a value of rho by setting rho rank in the     *)
  (* datafile. If it is set, use it.  Otherwise prompt the user to      *)
  (* enter it interactively.                                            *)
  If[rhoRank =!= Null,
    userDesignatedRank = rhoRank,

  (* else, no value for rhoRank requires prompt for rank. *)
    Print["An input window should appear asking the user to enter the rank."<>
        "  If no input box appears, it may be hiding behind the current"<>
        " window.  Lower or reduce the window to find it."];
    If[indepans === 1,
      While[!NumberQ[userDesignatedRank],
        userDesignatedRank = Input["Enter the RANK to use for calculating"<>
            " the density.  The rank for the density should be an integer"<>
            " multiple of the lowest weight of the DEPENDENT variable(s)."<>
            " Fractional ranks are allowed."]
      ]; (* end While *),
    (* else *)
      While[!NumberQ[userDesignatedRank],
        userDesignatedRank = Input["Enter the RANK to use for calculating"<>
            " the density.  The rank for the density should be an integer"<>
            " multiple of the lowest weight of the DEPENDENT variable(s)."<>
            " Fractional and negative ranks are allowed."]
      ]; (* end While *)
    ] (* end If indepans *)
  ]; (* end If ValueQ *)

  Print["==================================================================="];
  Print["Attempting to find conservation law(s) for RANK "<>
      "", userDesignatedRank, If[indepans === 2, " where terms have"<>
      " explicit "<> ToString[indepVarListWitht] <>".", "."]];
  Print["------------------------------------------"];

  (* Give warning if the density given is not an integer.               *)
  If[Head[userDesignatedRank] =!= Integer,
    printLOW["Warning:  Fractional ranks will only work if the numerical"<>
        " weight of one of the dependent variables is a corresponding"<>
        " fraction."]
  ]; (* end If userDesignatedRank *)
  printBRW["At BRW 1, the rank given is ", userDesignatedRank];

  (* Keep track of original rank given.                                 *)
  originalRank = userDesignatedRank;

  (* Scale weights if fractional weights exist.                         *)
  printBRW["At BRW 2, False means fractions exist in the weight list:"];
  printBRW[FreeQ[numericalWeightListIn, Rational]];
  If[Not[FreeQ[numericalWeightListIn, Rational]],
    {userDesignatedRank, numericalWeightList} =
        scaleWeightsIfFractions[userDesignatedRank, numericalWeightListIn];
    printBRW["At BRW 3, rescaling has occurred because of fraction",
        " weights.  The numerical weights are now:"];
    printBRW[numericalWeightList],

  (* else *)
    {userDesignatedRank, numericalWeightList} = {userDesignatedRank,
        numericalWeightListIn}
  ]; (* end If Not *)

  If[indepans === 1,
    (* Compute the list of terms that will form a differential function *)
    (* representing the conserved densities.  All terms generated with  *)
    (* buildFullRho will NOT contain any explicit independent variables.*)
    (* The list given here excludes any terms that are divergences or   *)
    (* divergence-equivalent.                                           *)
    rhoInBuckets = buildFullRho[numDepVars, weightedParameters,
        numericalWeightList, symbolicWeightListIn, indepVarList,
        userDesignatedRank];
    printBRW["At BRW 4, a list of terms to form rho is:"];
    printBRW[rhoInBuckets]
  ]; (* end If indepans === 1*)

  If[indepans === 2,
    (* Compute the list of terms that will form a differential function *)
    (* representing the conserved densities.  All terms generated with  *)
    (* attachExplicitVars DO contain explicit independent variables.   *)
    (* The list given here excludes any terms that are divergences or   *)
    (* divergence-equivalent.                                           *)
    rhoInBuckets = attachExplicitVars[numericalWeightList,
         symbolicWeightListIn, numDepVars, indepVarList, weightedParameters,
	 userDesignatedRank, explicitInd];
    printBRW["At BRW 5, a list of terms to form rho is:"];
    printBRW[rhoInBuckets]
  ]; (* end If indepans === 2 *)

  (* Clear all local variables not being returned.                      *)
  Clear[indepans, indepVarListWitht, printBRW, userDesignatedRank,
      numericalWeightList];

  Return[{originalRank, rhoInBuckets}]
]; (* end Module buildRhoWrapper *)

(* ##########      Function: scaleWeightsIfFractions       ########## *)

(**********************************************************************)
(* scaleWeightsIfFractions[userSpecifiedRank, numericWeightList]      *)
(* Purpose: To scale the numerical weight values and the rank if      *)
(*          there are fractions involved.  Otherwise,                 *)
(*          makeDerivativeList and makeVariableList will not work.    *)
(* Authors: Lindsay Auble, Forrest Lunstrom, Maxi von Eye             *)
(* Input:   A numeric weight list                                     *)
(*          The designated rank                                       *)
(* Output:  Scaled numeric weight list and user specified rank        *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whscwefr.m, Created 9 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpscwefr.m                      *)
(* Major Changes Made: 30 May, 2006, 9:35 by DP at CSM                *)
(* Last Modified:  1 October, 2006, 17:40 by DP at home               *)
(**********************************************************************)

If[debugScaleWeightsIfFractions, printSWIF = Print, Clear[printSWIF],
    Clear[printSWIF]];

scaleWeightsIfFractions[userSpecifiedRank_, numericWeightList_List]:=
Module[{fractionList = {}, a1, scaleNeeded, userRank, numWeightList, printSWIF},

  If[debugScaleWeightsIfFractions, printSWIF = Print, Clear[printSWIF],
      Clear[printSWIF]];

  printSWIF["debug code: SWIF, Function: scaleWeightsIfFractions,"<>
      " File: dpscwefr.m"];

  printSWIF["At SWIF IN, a list of numeric weights:"];
  printSWIF[numericWeightList];
  printSWIF["At SWIF IN, the designated rank is ", userSpecifiedRank];

  (* Make a list of the fractions from the numerical weight rules.      *)
  fractionList = Map[Part[#, 2]&, numericWeightList];
  printSWIF["At SWIF 1, a list of fractions from the numeric weights:"];
  printSWIF[fractionList];

  (* Check for fractions in the numerical weights, and make a list of   *)
  (* the denominators.                                                  *)
  fractionList = Cases[fractionList, Rational[_, _]];
  printSWIF["At SWIF 2, a list of weights that are fractions:"];
  printSWIF[fractionList];

  fractionList = fractionList /. {Rational[_, a1_] -> a1};
  printSWIF["At SWIF 3, a list of denominators from the list of fractions:"];
  printSWIF[fractionList];

  fractionList = fractionList /. List -> Sequence;

  (* Find the least common multiple to get rid of fractions.            *)
  scaleNeeded = LCM[fractionList];
  printSWIF["At SWIF 4, the least common denominator is ", scaleNeeded];

  (* Scale the specified rank.                                          *)
  userRank = userSpecifiedRank * scaleNeeded;

  (* Scale all the weights.                                             *)
  numWeightList = Map[Rule[Part[#, 1], scaleNeeded * Part[#, 2]] &,
      numericWeightList];
  printSWIF["At SWIF 5, after scaling the rank is ", userRank, " and"<>
      " the numerical weights are"];
  printSWIF[numWeightList];

  (* Clear all local variables not being returned.                      *)
  Clear[fractionList, a1, scaleNeeded, printSWIF];

  (* Return new user rank and weight list. *)
  Return[{userRank, numWeightList}]
]; (* end Module scaleWeightsIfFractions *)

(* ##########             Function: reduceRho              ########## *)

(**********************************************************************)
(* reduceRho[largeRho, symbolicWeightRules, weightedParameters]       *)
(* Purpose: To reduce the large rho by putting each term into         *)
(*          separate buckets (from scaling symmetries) that can be    *)
(*          treated as separate rhos.                                 *)
(* Input:   A complete list of density terms                          *)
(*          A list of symbolic weight rules                           *)
(*          A list of weighted parameters                             *)
(* Output:  A list of groups of density terms. Each group is placed   *)
(*          in its own sublist.                                       *)
(*          An accompanying list of how the terms were ranked         *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whredrho.m, Created 26 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpredrho.m                      *)
(* Last Modified:  28 April, 2008, 17:19 by DP at CSM                 *)
(**********************************************************************)

reduceRho[largeRho_List, symbolicWeightRules_List, weightedParameters_]:=
Module[{rules, count, a, b, c, f, rhoRankList, pairList, lengthRho,
    finalRhoList, ii, term, printRR},

  If[debugReduceRho, printRR = Print, Clear[printRR], Clear[printRR]];

  printRR["debug code: RR, Function: reduceRho, File: dpredrho.m"];

  printRR["At RR IN, the list of symbolic weight rules:"];
  printRR[symbolicWeightRules];
  printRR["At RR IN, tThe large rho list is:"];
  printRR[largeRho];
  printRR["At RR IN, the weighted parameters list contains:"];
  printRR[weightedParameters];

  Clear[weight, d, dt, dx, dy, dz];

  (* Rules to be applied to differential expressions to form weight     *)
  (* expressions in the function generateWeightOfTermsList.             *)
  (* These rules use Dr. Hereman's method of changing the product of    *)
  (* variables into the sum of the weights of the variables.            *)
  (* e.g., Each u[i] goes to E^w(u[i] and                               *)
  (* Each D[u[i], {x, m}, {y,n}] goes to E^(n*w(Dx)+m*w(Dy)+w(u[i])     *)
  rules = Union[Table[Part[weightedParameters, count] ->
      E^weight[Part[weightedParameters, count]],
          {count, 1, Length[weightedParameters]}],
      {u[count_Integer][__] -> E^weight[u[count]],
      Derivative[a_][u[count_Integer]][__] ->
          E^(a*weight[d/dx]+weight[u[count]]),
      Derivative[a_,f_][u[count_Integer]][__] ->
          E^(weight[u[count]] + a*weight[d/dx] + f*weight[d/dt]),
      Derivative[a_,b_,f_][u[count_Integer]][__] ->
          E^(weight[u[count]] + a*weight[d/dx] + b*weight[d/dy] +
                     f*weight[d/dt]),
      Derivative[a_,b_,c_,f_][u[count_Integer]][__] ->
          E^(weight[u[count]] + a*weight[d/dx] + b*weight[d/dy] +
                     c*weight[d/dz] + f*weight[d/dt])
      } (* end second argument for Union *)
    ];  (* end Union *)
  printRR["At RR 1, Rules applied to expression:"];
  printRR[rules];

  (* GenerateWeightOfTermsList creates a list of weight expressions.     *)
  printIWRF[StyleForm["Entering generateWeightOfTermsList",
            FontSlant->Italic]];
  rhoRankList = Map[generateWeightOfTermsList[#, weightedParameters,
      rules, False] &, largeRho];
  printIWRF[StyleForm["Exiting generateWeightOfTermsList",
            FontSlant->Italic]];
  printRR["At RR 2, a list of symbolic ranks for the rho terms:"];
  printRR[rhoRankList];

  (* Use the symbolic weight rules to rewrite rules in terms of          *)
  (* free weights.                                                       *)
  rhoRankList = Expand[Flatten[rhoRankList /. symbolicWeightRules]];
  printRR["At RR 3, a list of symbolic ranks for the rho terms after"<>
      " replacement with symbolic rules:"];
  printRR[rhoRankList];

  (* Make a list of lists containing pairs with the first element being     *)
  (* the term, and the second element being the symbolic rank of that term. *)
  pairList = Table[{Part[largeRho, ii], Part[rhoRankList, ii]},
      {ii, 1, Length[rhoRankList]}];
  printRR["At RR 4, a list of rho terms with their symbolic ranks:"];
  printRR[pairList];

  (* Making buckets.                                                     *)
  rhoRankList = Union[rhoRankList];
  lengthRho = Length[rhoRankList];

  printRR["At RR 5 , a shortened list of symbolic ranks:"];
  printRR[rhoRankList];
  printRR["At RR 5a, there ", If[lengthRho === 1, "is ", "are "],
      lengthRho, " symbolic weight", If[lengthRho === 1, ".", "s."]];

  (* Assigning terms to the different buckets.                           *)
  finalRhoList = Table[Cases[pairList, {term_, symbolicRank_}/;
      symbolicRank == rhoRankList[[ii]] -> term], {ii, 1, lengthRho}];

  For[ii = 1, ii <= lengthRho, ii++,
    printRR["At RR 6, for symbolic weight, ", Part[rhoRankList, ii],
        ",", " a density may contain the terms "];
    printRR[Part[finalRhoList,ii]]
  ]; (* end For *)

  printRR["At RR OUT the candidate density has been separated into buckets:"];
  printRR[finalRhoList];

  (* Clear all local variables not being returned.                      *)
  Clear[rules, count, a, b, c, f, pairList, lengthRho, ii, term, printRR];

  Return[{finalRhoList, rhoRankList}]
]; (* end Module reduceRho *)

(* ##########     Mini-function: removeEmptyBuckets        ########## *)

(**********************************************************************)
(* removeEmptyBuckets[bucketList, bucketRank]                         *)
(* Purpose: To remove empty lists from the final list of rhos.        *)
(* Input:   A list of densities in a bucket                           *)
(*          A list of symbolic ranks of densities                     *)
(* Output:  A list of densities with all empty lists deleted          *)
(*          A corresponding list of symbolic ranks with ranks deleted *)
(*              where there are no longer any densities               *)
(* Created: 2 July 2006 by DP at home                                 *)
(* Code is in File:  conservationlaws/nwbldrho.m                      *)
(* Last Modified:  28 April, 2008, 17:23 by DP at CSM                 *)
(**********************************************************************)

removeEmptyBuckets[bucketList_List, bucketRank_List]:=
Module[{emptyBucketAt, bucketListNew, bucketRankNew},

  emptyBucketAt = Position[bucketList, {}, {1}];
  bucketListNew = Delete[bucketList, emptyBucketAt];
  bucketRankNew = Delete[bucketRank, emptyBucketAt];

  Clear[emptyBucketAt];

  Return[{bucketListNew, bucketRankNew}]
]; (* end Module removeEmptyBuckets *)

(* ##########           Function: buildFullRho             ########## *)

(**********************************************************************)
(* buildFullRho[numDepVars, weightedParameters, numericWeightList,    *)
(*    symbolicWeightListIn, indepVarList, rankOfRhoDesired]           *)
(* Purpose: To construct a full list of terms to be used in           *)
(*          formulating densities.  This list is based on weights     *)
(*          of dependent variables, weighted parameters and the       *)
(*          derivatives with respect to the independent variables.    *)
(* Input:   The number of dependent variables as an integer           *)
(*          A list of weighted parameters                             *)
(*          A list of numeric weights for dependent variables and     *)
(*               weighted parameters                                  *)
(*          A list of symbolic weights                                *)
(*          A list of independent variables                           *)
(*          Rank on which to build the density                        *)
(* Output:  A list of candidate densities with undetermined           *)
(*              coefficientsfor the designated rank                   *)
(* Created: 2 July 2006 by DP at home                                 *)
(* Code is in File:  conservationlaws/nwbldrho.m                      *)
(* Last Modified:  28 April, 2008, 18:06 by DP at CSM                 *)
(**********************************************************************)

buildFullRho[numDepVars_, weightedParameters_, numericWeightList_,
    symbolicWeightListIn_, indepVarList_, rankOfRhoDesired_] :=
Module[{initialRhoList, indVar = Length[indepVarList], derivativeWeightRule,
    termWeightRule, weightedParametersRule, n1, n2, n3, dvar, weightList,
    initialWeightsOfTermsList, rankBeforeLastDerivativeIndVarList, ii,
    deficientRankList, weightsOfTermsList, amountOfRankDeficient,
    derivativesNeededList, derivativesForDeficientTerms, printBFRL,
    numCombinationsPerTermList, deficientTermListExpanded, dummytable,
    dummyD, dummypos, dummyext, derivativesApplied, i1,
    finalDerivativeList, trivialListTotalDer, termOrderList, maxTermOrderList,
    positionMaxTermOrderList, trivialList = {}, formRhoList = {},
    rhoTermsFullRank, rhoInBuckets, rhoRankList, workingList, strippedList,
    testDuplicate, duplicatePositions, fullPositionList = {}, printBFR},

  If[debugBuildFullRho, printBFR = Print, Clear[printBFR],  Clear[printBFR]];
  If[debugBuildFullRhoList, printBFRL = Print, Clear[printBFRL],
      Clear[printBFRL]];

  printBFR["debug code: BFR, Function: buildFullRho, File: nwbldrho.m"];

  printBFR["At BFR IN, the list of symbolic weights given:"];
  printBFR[symbolicWeightListIn];
  printBFR["At BFR IN, the list of numeric weights given:"];
  printBFR[numericWeightList];
  printBFR["At BFR IN, the number of dependent variables is ", numDepVars];
  printBFR["At BFR IN, the list of independent variables:", indepVarList];
  printBFR["At BFR IN, the list of weighted parameters:", weightedParameters];
  printBFR["At BFR IN, the rank for which the density is to be constructed: ",
      rankOfRhoDesired];

  Clear[weight, d, dt, dx, dy, dz];

  (* Set up initial list of terms for rho by combining weighted          *)
  (* dependent variables and weighted parameters.                        *)
  initialRhoList = constructInitialRhoTerms[numDepVars, weightedParameters,
      numericWeightList, rankOfRhoDesired, indepVarList];
  printBFR["At BFR 1, the length of the initial list of terms for",
      " rho provided by the function constructInitialRhoTerms is ",
      Length[initialRhoList]];
  printBFRL["At BFR 1a, the initial list of terms for rho provided",
      " by the function constructInitialRhoTerms is:"];
  printBFRL[initialRhoList];

  (* If there are no terms in the initial list, skip to the end of the   *)
  (* function.                                                           *)
  If[initialRhoList =!= {},

    (* Set up rules for determing weights of terms.                      *)
    derivativeWeightRule =
      {Derivative[n1_, _][u[dvar_]][x, t] -> n1*weight[d/dx] + weight[u[dvar]],
      Derivative[n1_, n2_, _][u[dvar_]][x, y, t] -> n1*weight[d/dx] +
      n2*weight[d/dy] + weight[u[dvar]],
      Derivative[n1_, n2_, n3_, _][u[dvar_]][x, y, z, t] -> n1*weight[d/dx] +
      n2*weight[d/dy] + n3*weight[d/dz] + weight[u[dvar]]};
    termWeightRule = {u[dvar_][__] -> weight[u[dvar]]};
    weightedParametersRule = Map[Rule[#, weight[#]]&, weightedParameters];
    printBFR["At BFR 2, the rules used for determining weights are"<>
        " given as:\nWeight rule for derivatives:"];
    printBFR[derivativeWeightRule];
    printBFR["Weight rule for dependent variables:"];
    printBFR[termWeightRule];
    printBFR["Weight rule for weighted parameters:"];
    printBFR[weightedParametersRule];

    (* Find the weights of terms in the current rho list so that terms   *)
    (* with rank less than the desired rank can be adjusted by applying  *)
    (* derivatives.                                                      *)
    initialWeightsOfTermsList = initialRhoList /. Times -> Plus /.
        Power -> Times /. derivativeWeightRule /. termWeightRule /.
        weightedParametersRule /. numericWeightList;
    printBFRL["At BFR 3, the weights on the rho terms are:"];
    printBFRL[initialWeightsOfTermsList];

    (* For each independent variable, determine the highest rank each     *)
    (* term in the rho list can be before performing the last derivative. *)
    weightList = Flatten[{weight[d/dx],
     If[indVar > 1, weight[d/dy],{}],
     If[indVar > 2, weight[d/dz], {}], {}}] /. numericWeightList;
    rankBeforeLastDerivativeIndVarList = Table[rankOfRhoDesired,
       {i1, 1, indVar}] - weightList;
    printBFR["At BFR 4, a list of weights for the variable(s) is ",
       weightList];
    printBFR["Before performing the last x-derivative, all terms need"<>
       " to have a rank of ", Part[rankBeforeLastDerivativeIndVarList, 1]];
    If[indVar > 1, printBFR["Before performing the last",
       " y-derivative, all terms need to have a rank of ",
       Part[rankBeforeLastDerivativeIndVarList, 2]]];
    If[indVar > 2, printBFR["Before performing the last",
       " z-derivative, all terms need to have a rank of ",
       Part[rankBeforeLastDerivativeIndVarList, 3]]];

    (* Begin adjustment of rank by applying all possible combinations    *)
    (* of derivatives to the terms that have a rank less than the        *)
    (* desired rank.                                                     *)
    While[indVar >= 1,
      printBFR["Building derivative list.  ",
          "Working with independent variable ", Part[indepVarList, indVar],
          "."];

      (* Separate the initial list of terms for rho into a list of terms *)
      (* with the desired rank and a list of terms with less than the    *)
      (* desired rank. *)
      deficientRankList = Extract[initialRhoList,
          Position[initialWeightsOfTermsList,
          _?(# <= rankOfRhoDesired - Part[weightList, indVar] &) ]];

      (* Eliminate terms with stand alone weighted parameters from the   *)
      (* list of terms with less than the desired rank.                  *)
      deficientRankList = DeleteCases[deficientRankList,
          _?(MemberQ[#, x, Infinity] === False &)];
      printBFR["At BFR 5, a list of terms that have rank ",
          rankOfRhoDesired - Part[weightList, indVar], " whose rank can be",
          " adjusted by applying derivatives consists of ",
          Length[deficientRankList], " terms."];
      printBFRL["At BFR 5a, the rho terms that have a rank less than",
          " the desired rank that can be adjusted by applying derivatives:"];
      printBFRL[deficientRankList];

      (* Determine the amount the rank must be adjusted for each term in *)
      (* the deficient rank list.                                        *)
      weightsOfTermsList = deficientRankList /. Times -> Plus /.
         Power -> Times /. derivativeWeightRule /. termWeightRule /.
         weightedParametersRule /. numericWeightList;
      (* The addition of 1 in the next line adjusts the table so that    *)
      (* the ranks can be mapped with the derivatives needed list below. *)
      amountOfRankDeficient =
         Table[Part[rankBeforeLastDerivativeIndVarList, indVar],
               {i1, 1, Length[weightsOfTermsList]}] - weightsOfTermsList + 1;
      printBFR["At BFR 6, each term in the deficient list needs to be",
         " adjusted by the corresponding rank in the following list:"];
      printBFR[amountOfRankDeficient - 1];

      (* Find the combinations of derivatives needed to bring the rank of    *)
      (* each term up to the rank desired before taking the last derivative. *)
      derivativesNeededList =
          Table[determineDerivativesNeeded[Take[weightList, indVar], i1],
                {i1, 0, Part[rankBeforeLastDerivativeIndVarList, indVar] - 1}];
      printBFR["At BFR 7, combinations of derivatives to change the rank"<>
          " of deficient terms by up to ", Length[derivativesNeededList],
          " are given in a form that can be easily mapped into D"<>
          " (the derivative function):"];
      printBFR[derivativesNeededList];

      (* Set up to apply the needed derivative(s) to each term. Since   *)
      (* more than one combinatiion of derivatives may be applied to    *)
      (* each term, the lists are adjusted accordingly.                 *)
      derivativesForDeficientTerms = Map[Part[derivativesNeededList, #]&,
                 amountOfRankDeficient];
      printBFRL["At BFR 8, derivatives needed for deficient terms:"];
      printBFRL[derivativesForDeficientTerms];
      numCombinationsPerTermList = Map[Length, derivativesForDeficientTerms];
      deficientTermListExpanded = Thread[dummytable[deficientRankList,
        Map[{ii, 1, #} &, numCombinationsPerTermList]]] /. dummytable -> Table;
      printBFRL["At BFR 9, the deficient term list in expanded form:"];
      printBFRL[deficientTermListExpanded];

      (* Take the derivatives.                                          *)
      derivativesApplied =
         Expand[Thread[dummyD[Flatten[deficientTermListExpanded],
         Flatten[derivativesForDeficientTerms]]] /.
                 dummyseq -> Sequence /. dummyD -> D];
      printBFR["At BFR 10 ", Length[derivativesApplied], " terms have",
          " been produced after applying the combinations of derivatives",
          " needed to create terms of rank ",
          Part[rankBeforeLastDerivativeIndVarList, indVar], "; ",
          " the rank needed before applying the last derivative."];
      printBFRL["At BFR 10, after the derivatives have been applied,",
          " the list of terms with rank ",
          Part[rankBeforeLastDerivativeIndVarList, indVar], " is:"];
      printBFRL[derivativesApplied];

      (* Apply stripper to the previous list.                          *)
      derivativesApplied = Flatten[Map[stripper[#, weightedParameters]&,
          derivativesApplied]];
      printBFRL["At BFR 11 after the stripper has been applied, ",
          " the list of terms with rank ",
          Part[rankBeforeLastDerivativeIndVarList, indVar], " is:"];
      printBFRL[derivativesApplied];
      printBFRL["Checking the rank of each term:"];
      printBFRL[derivativesApplied /. Times -> Plus /. Power -> Times /.
          derivativeWeightRule /. termWeightRule /. weightedParametersRule /.
          numericWeightList];

      (* Apply the final derivative needed to bring the terms in the    *)
      (* list up to the desired rank.                                   *)
      finalDerivativeList = Map[D[#, Part[indepVarList, indVar]]&,
          derivativesApplied];
      printBFR["At BFR 12, ", Length[Flatten[finalDerivativeList]],
          " terms exist after applying the final derivative to reach a ",
          " rank of ", rankOfRhoDesired];
      printBFRL["At BFR 12a, after the final ", Part[indepVarList, indVar],
          "-", "derivative has been applied, the list of terms with rank ",
          rankOfRhoDesired, " is:"];
      printBFRL[finalDerivativeList];

      (* Again, apply the stripper to remove all coefficients.           *)
      finalDerivativeList = Map[stripper[#, weightedParameters]&,
          finalDerivativeList];
      printBFRL["At BFR 13, after the derivatives have been stripped,",
          " the list of terms with rank ", rankOfRhoDesired, " is:"];
      printBFRL[finalDerivativeList];
      printBFRL["Checking the rank of each term:"];
      printBFRL[finalDerivativeList /. Times -> Plus /. Power -> Times /.
      derivativeWeightRule /. termWeightRule /. weightedParametersRule /.
      numericWeightList];

      (* Remove all total derivatives from the list of final            *)
      (* derivatives.                                                   *)
      (* Total derivatives are single term expressions in the final     *)
      (* derivative list.                                               *)
      (* trivialList is constructed of terms that are removed either    *)
      (* as total derivatives or as divergence-equivalents.             *)
      trivialListTotalDer = Cases[finalDerivativeList, _?(Length[#] === 1 &)];
      finalDerivativeList = Complement[finalDerivativeList,
                                       trivialListTotalDer];
      printBFR["At BFR 14, ", Length[trivialListTotalDer],
          " total derivatives have been removed from the list. There are ",
          Length[Flatten[finalDerivativeList]], " terms in the final"<>
          " derivative list left."];
      printBFRL["At BFR 14a, after removing all of the total"<>
          " derivatives, the final derivative list consists of:"];
      printBFRL[finalDerivativeList];
      printBFRL["and the trivial list contains:"];
      printBFRL[trivialListTotalDer];

      (* Remove divergence-equivalent terms, keeping the lowest order   *)
      (* terms.                                                         *)
      termOrderList = finalDerivativeList /. Power[m_, n_] -> Power[m, 1] /.
          Times -> Max /.
          {Derivative[m__][u[_]][__] -> Derivative[m], u[_][__] -> 1} /.
          Derivative -> Plus /. Map[Rule[#, 1] &, weightedParameters];
      printBFRL["At BFR 15, maximum orders for each term in the final",
          " derivative list:"];
      printBFRL[termOrderList];

      (* Find the largest order terms and place them in the trivial     *)
      (* terms list.                                                    *)
      maxTermOrderList = Apply[Max, termOrderList, {1, 1}];
      printBFRL["At BFR 16, maxima of the minilists shown at BFR 10:"];
      printBFRL[maxTermOrderList];
      positionMaxTermOrderList = Map[Last, Thread[dummypos[termOrderList,
          maxTermOrderList]] /. dummypos -> Position];
      printBFRL["At BFR 17, positions for the maximum term in each",
          " minilist shown at BFR 10:"];
      printBFRL[positionMaxTermOrderList];
      trivialList = Union[Flatten[trivialListTotalDer],
          Flatten[Append[trivialList,
          Thread[dummyext[finalDerivativeList, positionMaxTermOrderList]] /.
          dummyext -> Extract]]];
      printBFR["At BFR 18, the trivial list now contains all total",
          " derivatives and divergence-equivalent terms from the final",
          " derivative list.  There are ", Length[trivialList], " terms."];
      printBFRL["At BFR 18a, the trivial list consisting of all",
          " total derivatives and divergence-equivalent terms taken from"<>
          " the final derivative list is:"];
      printBFRL[trivialList];
      formRhoList = Complement[Union[formRhoList,
          Flatten[finalDerivativeList]], trivialList];
      printBFR["At BFR 19, ", Length[formRhoList], " terms with"<>
          " derivative elements are now in list of terms for rho."];
      printBFRL["At BFR 19a, a list of terms to be added to the",
          " list of rho terms:"];
      printBFRL[formRhoList];
      indVar--;
    ]; (* end While *)

    (* Pick out all nonderivative terms that have the desired rank and   *)
    (* combine them with terms found by applying derivatives.            *)
    rhoTermsFullRank = Extract[initialRhoList,
        Position[initialWeightsOfTermsList, _?(# === rankOfRhoDesired &)]];
    printBFR["At BFR 20 there are ", Length[rhoTermsFullRank], " that",
        " have full rank in the original list."];
    printBFRL["At BFR 20, a list of terms for rho with rank ",
        rankOfRhoDesired, " is:"];
    printBFRL[rhoTermsFullRank];
    formRhoList = Union[formRhoList, rhoTermsFullRank];
    printBFR["At BFR 21 the number of terms in the rho list are"<>
        " currently ", Length[formRhoList]];
    printBFRL["At BFR 21a the terms in the rho list are currently:"];
    printBFRL[formRhoList];

    (* Put terms of rho into seperate buckets                            *)
    (* rhoInBuckets is a list of lists.  The first sublist is the        *)
    (* different rhos, and the second sublist is the different symbolic  *)
    (* weights of the corresponding rhos                                 *)
    If[symbolicWeightListIn =!= {},
      {rhoInBuckets, rhoRankList} = reduceRho[formRhoList,
       symbolicWeightListIn, weightedParameters];
      printBFR["At BFR 22, various rhos can be formed using these"<>
          " groupings:"];
      printBFR[rhoInBuckets],
    (* else *)
      {rhoInBuckets, rhoRankList} = {{formRhoList}, {}}
    ]; (* end If symbolicWeightListIn *)

    (* Check further for total derivatives and divergence-equivalent     *)
    (* terms using the variational derivative.                           *)
    formRhoList = Map[reduceRhoEuler[#, numDepVars, indepVarList] &,
        rhoInBuckets];
    printBFR["At BFR 23, the number of buckets in the rho list after"<>
        " reduceRhoEuler are currently ", Length[formRhoList]];
    printBFR["At BFR 23a, the total number of terms in the rho list"<>
        " after reduceRhoEuler are currently ", Length[Flatten[formRhoList]]];
    printBFRL["At BFR 23b, the terms in the rho list after reduceRhoEuler"<>
        " are currently:"];
    printBFRL[formRhoList];

    (* Remove any empty buckets from the list.                          *)
      {formRhoList, rhoRankList} =
      removeEmptyBuckets[formRhoList, rhoRankList];
    printBFR["At BFR 24, the number of buckets in the rho list after"<>
        " removing empty buckets is ", Length[formRhoList]];
    printBFRL["At BFR 24a, the terms in the rho list after removing"<>
        " empty buckets:"];
    printBFRL[formRhoList];

    (* Check for duplicate rhos in the buckets, then remove them.  The  *)
    (* duplicates would be eliminated in evaluateRho, but elimination   *)
    (* here reduces the load on the General Solver.                     *)
    workingList = strippedList = Map[stripper, formRhoList, {2}];
    strippedList = Apply[Sequence, strippedList, {2}];
    printBFR["At BFR 25, the stripped rho list:"];
    printBFR[strippedList];
    workingList = Apply[Sequence, Union[workingList], {2}];
    printBFR["At BFR 26, the list of terms to check for duplicates in"<>
        " consists of:"];
    printBFR[workingList];
    While[workingList =!= {},
      testDuplicate = First[workingList];
      printBFR["At BFR 27, the working list is being checked for duplicate"<>
          " terms of: ", testDuplicate];
      workingList = Drop[workingList, 1];
      duplicatePositions =
               Drop[Position[strippedList, testDuplicate, {1}], 1];
      printBFR["At BFR 28, duplicates of ", testDuplicate, " were found in"<>
          " the rho list at positions:"];
      printBFR[duplicatePositions];
      fullPositionList = Union[fullPositionList, duplicatePositions];
      printBFR["At BFR 29, duplicate terms exist in the rho list at"<>
          " positions:"];
      printBFR[fullPositionList]
    ]; (* end While *)
    formRhoList = Delete[formRhoList, fullPositionList];
    printBFR["At BFR 30, the total number of terms in the rho list" <>
        " after removing duplicate terms: ", Length[formRhoList]];
    printBFRL["At BFR 30a, the terms in the rho list after removing" <>
        " duplicate terms:"];
    printBFRL[formRhoList];

    (* Again it is necessary to remove empty buckets from the rho.      *)
    (* Removal of duplicates leaves empty lists behind.                 *)
    {formRhoList, rhoRankList} = removeEmptyBuckets[formRhoList, rhoRankList];
    printBFR["At BFR 31, the number of buckets in the rho list after"<>
        " removing empty buckets is ", Length[formRhoList]];
    printBFRL["At BFR 31a, the terms in the rho list after removing"<>
        " empty buckets:"];
    printBFRL[formRhoList]
  ]; (* end If initialRhoList *)

  printBFR["At BFR OUT, a list of candidate densities:"];
  printBFR[formRhoList];

  Clear[initialRhoList, indVar, derivativeWeightRule,  termWeightRule,
      weightedParametersRule, n1, n2, n3, dvar, weightList, termOrderList,
      initialWeightsOfTermsList, rankBeforeLastDerivativeIndVarList,
      deficientRankList, weightsOfTermsList, amountOfRankDeficient,
      derivativesNeededList, derivativesForDeficientTerms, printBFRL,
      numCombinationsPerTermList, deficientTermListExpanded, i1,
      derivativesApplied, finalDerivativeList, trivialListTotalDer,
      positionMaxTermOrderList, trivialList, rhoTermsFullRank, rhoInBuckets,
      rhoRankList, workingList, strippedList, duplicatePositions,
      maxTermOrderList, testDuplicate, fullPositionList, printBFR];

  Return[formRhoList]
]; (* end Module buildFullRho *)

(* ##########      Function: constructInitialRhoTerms      ########## *)

(**********************************************************************)
(* constructInitialRhoTerms[numDepVars, weightedParameters,           *)
(*                        numericWeightList, rankOfRho, indepVarList] *)
(* Purpose: To construct a list of terms to be used in formulating    *)
(*          densities.  This list is based on weights of dependent    *)
(*          variables and weighted parameters.                        *)
(* Input:   Number of dependent variables as an integer               *)
(*          List of weighted parameters                               *)
(*          List of numeric weights for dependent variables and       *)
(*               weighted parameters                                  *)
(*          Rank on which to build the density                        *)
(*          List of independent variables                             *)
(* Output:  List of terms involving the dependent variables and       *)
(*              weighted parameters of desired rank or less           *)
(* Created: 26 May 2006 by DP  at CSM                                 *)
(* Code is in File:  conservationlaws/nwconrho.m                      *)
(* Last Modified:  29 April, 2008, 10:29 by DP  at CSM                *)
(**********************************************************************)

constructInitialRhoTerms[numDepVars_, weightedParameters_,
    numericWeightList_, rankOfRho_, indepVarList_] :=
Module[{partialWeightRules, depParamWeightRules, zeroWeights, weightRules,
    numericWeightListNoZeros, depVarsandParams, depVarsandParamsOrig,
    endWhile = 0, weightsOfTermsList, rhoTermsList = {}, indepVarListAndt,
    printCIRT},

  If[debugConstructInitialRhoTerms, printCIRT = Print, Clear[printCIRT],
      Clear[printCIRT]];

  printCIRT["debug code: CIRT, Function: constructInitialRhoTerms,"<>
      " File: nwconrho.m"];

  printCIRT["At CIRT IN the list of numeric weights given:"];
  printCIRT[numericWeightList];
  printCIRT["At CIRT IN the current rank is ", rankOfRho];
  printCIRT["At CIRT IN the number of dependent variables is ", numDepVars];
  printCIRT["At CIRT IN the list of independent variables is ", indepVarList];
  printCIRT["At CIRT IN the weighted parameter list is ", weightedParameters];

  printCIRT["Beginning the development of the list of rho terms"];

  (* Check weights on independent variables.                             *)
  partialWeightRules = Cases[numericWeightList, _?(Part[#, 1] ===
      weight[d/dx] || Part[#, 1] === weight[d/dy] ||
      Part[#, 1] === weight[d/dz] || Part[#, 1] === weight[d/dt] &)];

  (* If any dependent variables or weighted parameters have a weight of  *)
  (* zero, remove them.                                                  *)
  depParamWeightRules = Complement[numericWeightList, partialWeightRules];
  zeroWeights = Cases[depParamWeightRules, _?(Part[#1, 2] === 0 &)];
  numericWeightListNoZeros = If[zeroWeights =!= {},
       DeleteCases[depParamWeightRules, Apply[Sequence, zeroWeights]],
       depParamWeightRules, depParamWeightRules];
  printCIRT["At CIRT 1, terms with zero weights are ", zeroWeights];
  printCIRT["At CIRT 2, a list of weight rules for dependent variables",
      " and weighted parameters after terms with weight 0 have been removed:"];
  printCIRT[numericWeightListNoZeros];
  depVarsandParamsOrig = Union[Table[u[i], {i, 1, numDepVars}],
      weightedParameters];
  depVarsandParams = depVarsandParamsOrig = If[zeroWeights =!= {},
      DeleteCases[depVarsandParamsOrig,
      Apply[Sequence, Cases[zeroWeights, u[_], Infinity]]],
      depVarsandParamsOrig, depVarsandParamsOrig];
  printCIRT["At CIRT 3, a list of dependent variables with weighted",
      " parameters is"];
  printCIRT[ depVarsandParamsOrig];
  weightRules = numericWeightListNoZeros /. weight -> Log;
  printCIRT["At CIRT 4, a modified list of rules for calculating",
      " weights for dependent variables and weighted parameters is"];
  printCIRT[weightRules];

  (* Start building up terms by multiplying through the list of          *)
  (* dependent variables and weighted parameters.  Each turn through the *)
  (* loop creates terms with higher rank, while retaining terms already  *)
  (* constructed.  The resulting list will have all possible             *)
  (* combinations of weighted parameters and dependent variables up to   *)
  (* desired rank.                                                       *)
  While[endWhile === 0,
    weightsOfTermsList = PowerExpand[Map[Log, depVarsandParams]]
        /. weightRules;
    printCIRT["At CIRT 5, for the current dependent variables with",
        " weighted parameters list ", depVarsandParams,
        "\nthe weights are ", weightsOfTermsList];
    depVarsandParams = Delete[depVarsandParams,
        Position[weightsOfTermsList, _?(# > rankOfRho &)]];
    printCIRT["At CIRT 6, after removing over-rank terms, the dependent",
        " variables with weighted parameters list is:"];
    printCIRT[depVarsandParams];

    (* Form a combined list with current possibilities for rho.          *)
    rhoTermsList = Union[rhoTermsList, depVarsandParams];
    printCIRT["At CIRT 7, a list of terms for rho now consists of:"];
    printCIRT[rhoTermsList];
    weightsOfTermsList =
                  PowerExpand[Map[Log, depVarsandParams]] /. weightRules;

    (* Find all terms that have rank less than the desired rank.         *)
    depVarsandParams = Delete[depVarsandParams,
        Position[weightsOfTermsList, _?(# === rankOfRho &)]];
    printCIRT["At CIRT 8, a list of terms with rank less than ", rankOfRho,
        " that needs further adjustment is:"];
    printCIRT[depVarsandParams];

    (* Develop the next set of terms by multiplying each existing term   *)
    (* with less than the desired rank by terms from the dependent       *)
    (* variable and weighted parameters list.                            *)
    If[MemberQ[weightsOfTermsList, _?(# < rankOfRho &)],
        depVarsandParams = Union[Flatten[Thread[Times[depVarsandParams,
        Table[depVarsandParamsOrig, {i, 1, Length[depVarsandParams]}]]]]];
    printCIRT["At CIRT 9, after multiplying through the list, the",
        " dependent variables with weighted parameters list is:"];
    printCIRT[depVarsandParams], endWhile = 1, endWhile = 1
    ] (* end If MemberQ *)
  ]; (* end While *)

  (* Show the initial list of terms established for rho along with a     *)
  (* list of weights corresponding to the terms.                         *)
  weightsOfTermsList = PowerExpand[Map[Log, rhoTermsList]] /. weightRules;
  printCIRT["At CIRT 10, the list of rho terms excluding derivatives is:"];
  printCIRT[rhoTermsList];
  printCIRT["At CIRT 10a, a list of weights corresponding to the terms",
      " in the list above is"];
  printCIRT[weightsOfTermsList];

  (* Add the independent variables to the terms in the rho list.         *)
  indepVarListAndt = Flatten[{indepVarList, t}];
  rhoTermsList = rhoTermsList /.
      u[n_] -> u[n][Apply[Sequence, indepVarListAndt]];
  printCIRT["At CIRT 11, rho terms with independent variables:"];
  printCIRT[rhoTermsList];

  (* Clear all local variables not being returned.                      *)
  Clear[partialWeightRules, depParamWeightRules, zeroWeights, weightRules,
      numericWeightListNoZeros, depVarsandParams, depVarsandParamsOrig,
      endWhile, weightsOfTermsList, indepVarListAndt, printCIRT];

  Return[rhoTermsList]
]; (* end Module constructInitialRhoTerms *)

(* ##########     Function: determineDerivativesNeeded     ########## *)

(**********************************************************************)
(* determineDerivativesNeeded[weightPartialDerivativesList,           *)
(*                            desiredRank]                            *)
(* Purpose: To determine the number of derivatives to bring a term in *)
(*          the list of terms for rho up to one less derivative then  *)
(*          is needed to achieve full rank.                           *)
(* Input:   List of weights for derivatives with respect to each      *)
(*          variable                                                  *)
(*          Desired rank for the density being constructed            *)
(* Output:  List of derivatives to be applied to achieve the desired  *)
(*          rank in a form that can be mapped to the function D.      *)
(* Created: 30 June 2006 by DP at CSM                                 *)
(* Code is in File:  conservationlaws/nwdetder.m                      *)
(* Last Modified:  29 April, 2008, 11:04 by DP at CSM                 *)
(**********************************************************************)

determineDerivativesNeeded[weightPartialDerivativesList_List, desiredRank_]:=
Module[{numIndepVars = Length[weightPartialDerivativesList], numDerList,
    lowWeight, lowWeightPosition, lowWeightVar, highWeight, highWeightVar,
    m1, m2, m3, n1, n2, n3, highWeightPosition, mediumWeight, mediumWeightVar,
    printDDN},

  If[debugDetermineDerivativesNeeded, printDDN = Print, Clear[printDDN],
      Clear[printDDN]];

  printDDN["debug code: DDN, Function: determineDerivativesNeeded,"<>
      " File: nwdetder.m"];

  printDDN["At DDN IN, with"<>
      " weightPartialDerivativesList: ", weightPartialDerivativesList, ""<>
      " which means that" ];
  printDDN["weight[d/dx] = ", Part[weightPartialDerivativesList, 1]];
  If[numIndepVars > 1,
      printDDN["weight[d/dy] = ", Part[weightPartialDerivativesList, 2]]];
  If[numIndepVars > 2,
      printDDN["weight[d/dz] = ", Part[weightPartialDerivativesList, 3]]];
  printDDN["At DDN IN, this function will develop derivative combinations"<>
      " to reach a desired rank of ", desiredRank, "."];

  (* Determine number of derivatives needed on one independent variable  *)
  (* to bring a term up to the desired rank. Put the possible            *)
  (* combinations of derivatives in a list that can be mapped onto the   *)
  (* Derivative function.                                                *)
  If[numIndepVars === 1,
    numDerList = Flatten[Solve[m1*Part[weightPartialDerivativesList, 1] ==
        desiredRank, m1]];
    printDDN["At DDN 1, possible nth derivatives of x are:"];
    printDDN[numDerList];
    numDerList = numDerList /. Rule -> List;
    printDDN["At DDN 2, the previous rule written as a list:"];
    printDDN[numDerList];
    If[Head[Part[numDerList, 1, 2]] === Integer,
      numDerList = {numDerList /. m1 -> x},
    (* else *)
      numDerList = {}
    ]; (* end If Head[m1] *)
    printDDN["At DDN 3, the number of x-derivatives needed to change",
        "  the term to rank ", desiredRank, " is given as: ", numDerList]
  ]; (* end If numIndepVars === 1 *)

  (* Determine number of derivatives needed on two independent variables *)
  (* to bring a term up to the desired rank. Put the possible            *)
  (* combinations of derivatives in a list that can be mapped onto the   *)
  (* Derivative function.                                                *)
  If[numIndepVars === 2,
    lowWeight = Min[weightPartialDerivativesList];
    lowWeightPosition = Part[Position[weightPartialDerivativesList,
        lowWeight], 1];
    lowWeightVar = Part[Extract[{x, y}, {lowWeightPosition}], 1];
    highWeight = Part[Delete[weightPartialDerivativesList,
        {lowWeightPosition}], 1];
    highWeightVar = Part[Complement[{x, y}, {lowWeightVar}], 1];
    printDDN["At DDN 4, \nIndependent variable ", lowWeightVar,
        " with weight ", lowWeight,
        " has the smallest weight.\nIndependent variable ", highWeightVar,
        " with weight ", highWeight, " has the greatest weight."];
    numDerList =  Table[{Part[Solve[m1*lowWeight + m2*highWeight ==
        desiredRank, m1], 1, 1, 2], m2}, {m2, 0, Floor[desiredRank/highWeight]}];
    printDDN["At DDN 5, an initial list of needed derivatives"];
    printDDN[numDerList];
    numDerList = DeleteCases[numDerList, List[n1_, n2_] /;
        Head[n1] === Rational || Head[n2] === Rational];
    numDerList = Map[Thread[List[{lowWeightVar, highWeightVar}, #1]] &,
        numDerList];
    printDDN["At DDN 6, to reach the desired rank of ", desiredRank, ", ",
     " the following sets of derivatives need to be made on the current term"];
    printDDN[numDerList]
  ]; (* end If numIndepVars === 2 *)

  (* Determine number of derivatives needed on three independent         *)
  (* variables to bring a term up to the desired rank. Put the possible  *)
  (* combinations of derivatives in a list that can be mapped onto the   *)
  (* Derivative function.                                                *)
  If[numIndepVars === 3, lowWeight = Min[weightPartialDerivativesList];
    lowWeightPosition = Part[Position[weightPartialDerivativesList,
        lowWeight], 1];
    lowWeightVar = Part[Extract[{x, y, z}, {lowWeightPosition}], 1];
    highWeight = Max[weightPartialDerivativesList];
    highWeightPosition = Part[Position[weightPartialDerivativesList,
        highWeight], 1];
    highWeightVar = If[highWeightPosition =!= lowWeightPosition,
        Part[Extract[{x, y, z}, {highWeightPosition}], 1],
        Part[Extract[{x, y, z}, {{3}}], 1]];
    mediumWeight = Part[Delete[weightPartialDerivativesList,
        {lowWeightPosition, highWeightPosition}], 1];
    mediumWeightVar = Part[Complement[{x, y, z},
        {lowWeightVar, highWeightVar}], 1];
    printDDN["At DDN 7.\nIndependent variable ", lowWeightVar,
        " with weight ", lowWeight,
        " has the smallest weight.\nIndependent variable ", mediumWeightVar,
        " with weight ", mediumWeight, " has the middle weight.\nIndependent",
        " variable ", highWeightVar, " with weight ", highWeight, " has the",
        " greatest weight."];
    numDerList = Flatten[Table[{Part[Solve[m1*lowWeight + m2*mediumWeight +
        m3*highWeight == desiredRank, m1], 1, 1, 2],m2 , m3},
        {m3, 0,Floor[desiredRank/highWeight]},
        {m2, 0, Floor[desiredRank/mediumWeight]}], 1];
    numDerList = DeleteCases[numDerList, List[n1_, _, _] /; n1 < 0];
    printDDN["At DDN 8, an initial list of needed derivatives"];
    printDDN[numDerList];
    numDerList = DeleteCases[numDerList, List[n1_, n2_, n3_] /;
        Head[n1] === Rational || Head[n2] === Rational ||
        Head[n3] === Rational];
    numDerList =
       Map[Thread[List[{lowWeightVar, mediumWeightVar, highWeightVar},#]]&,
           numDerList];
    printDDN["At DDN 9, to reach the desired rank of ", desiredRank, ",",
        " the following sets of derivatives need to be made on the",
        " current term"];
    printDDN[numDerList]
  ]; (* end If numIndepVars === 3 *)

  (* dummyseq allows the list of derivatives to be mapped into the       *)
  (* Derivative term.  Derivative can be performed after dummyseq ->     *)
  (* Sequence.                                                           *)
  Clear[dummyseq];
  numDerList = Apply[dummyseq, numDerList, 1];
  printDDN["At DDN 10, the list after applying dummyseq:"];
  printDDN[numDerList];

  (* Clear all local variables not being returned.                      *)
  Clear[numIndepVars, lowWeight, lowWeightPosition, lowWeightVar, highWeight,
      highWeightVar, m1, m2, m3, n1, n2, n3, highWeightPosition, mediumWeight,
      mediumWeightVar, printDDN];

  Return[numDerList]
]; (* end Module determineDerivativesNeeded *)

(* ##########           Function: reduceRhoEuler           ########## *)

(**********************************************************************)
(* reduceRhoEuler[listOfRhoTerms, numDepVars, indepVars]              *)
(* Purpose: To find terms in list of prospective terms for rho that   *)
(*          divergences (total derivatives in 1D) or divergence-      *)
(*          equivalent by comparing the variational derivatives of    *)
(*          the terms.                                                *)
(* Input:   A list of prospective terms for the candidate density     *)
(*          The number of dependent variables                         *)
(*          A list of independent variables                           *)
(* Output:  The most economical list of terms for rho                 *)
(* Created: 15 July 2006 by DP at home                                *)
(* Code is in File:  conservationlaws/nwredeu3.m                      *)
(* Major modifications to original file nwredeul.m                    *)
(*          made 1 May, 2007 by DP                                    *)
(* Last Modified:  29 April, 2008, 13:09 by DP at CSM                 *)
(**********************************************************************)

reduceRhoEuler[listOfRhoTerms_, numDepVars_, indepVars_] :=
Module[{varDerivOnRhoTerms, positionOfDivergences, keepRhoTermsList,
    constantList, q, i1, workingVarDerList, strippedTerms, positionIntegers,
    loneConstants, coefficientSolns, revCoefficientSolns, nonZeroSolns,
    dependentTerms, n1, otherDependents, rul, removedTerms, printRRE},

  If[debugReduceRhoEuler, printRRE = Print, Clear[printRRE], Clear[printRRE]];

  printRRE["debug code: RRE, Function: reduceRhoEuler, File: nwredeul.m"];

  printRRE["At RRE IN, the list of rho terms given:"];
  printRRE[listOfRhoTerms];
  printRRE["At RRE IN, there are ", Length[listOfRhoTerms], " terms"<>
      " in this list."];
  printRRE["At RRE IN, the number of dependent variables is ", numDepVars];
  printRRE["At RRE IN, the list of independent variables is ", indepVars];

  (* Create a list of variational derivatives matching each of the      *)
  (* terms in the input rho list.                                       *)
  varDerivOnRhoTerms =
      Map[Table[variationalDerivativeMultiD[#, i1, indepVars],
      {i1, 1, numDepVars}] &, listOfRhoTerms];
  printRRE["At RRE 1, the variational derivatives corresponding to each"<>
      " of the density terms:"];
  printRRE[varDerivOnRhoTerms];

  (* Remove all divergences, i.e., terms where the variational          *)
  (* derivative is 0 for all components.                                *)
  positionOfDivergences = Position[varDerivOnRhoTerms,
      Table[0, {i1, 1, numDepVars}]];
  printRRE["At RRE 2, terms that are divergences and need to be"<>
      " removed from the list of rho terms are at positions:"];
  printRRE[positionOfDivergences];
  keepRhoTermsList = Delete[listOfRhoTerms, positionOfDivergences];
  printRRE["At RRE 3, after removing all divergences, there are "<>
      "", Length[keepRhoTermsList], " terms in the list for rho."];
  printRRE["At RRE 3a, after removing all divergences, the list of"<>
      " terms for rho consists of:"];
  printRRE[keepRhoTermsList];

  (* Exit function if all terms in the list are deleted.                *)
  If[keepRhoTermsList === {}, Return[keepRhoTermsList]];

  varDerivOnRhoTerms = Delete[varDerivOnRhoTerms,
      positionOfDivergences];
  printRRE["At RRE 3b, after removing all zero variational derivatives, the"<>
      " variational derivative list has ", Length[varDerivOnRhoTerms], ""<>
      " terms."];

  (* Remove all terms that are divergence equivalent.                    *)
  (* Terms found later in the list are removed.                          *)
  constantList = Table[q[i1], {i1, 1, Length[varDerivOnRhoTerms]}];
  printRRE["At RRE 4, a list of constants to be attached to the variational"<>
      " derivatives:"];
  printRRE[constantList];

  (* Add constants q[i], to the variatiational derivatives. *)
  workingVarDerList = Thread[Times[varDerivOnRhoTerms, constantList], {1}];
  printRRE["At RRE 5, a list of variational derivatives with constants"<>
      " attachted:"];
  printRRE[workingVarDerList];

  workingVarDerList = Expand[Apply[Plus, workingVarDerList]];
  printRRE["At RRE 6, variational derivatives combined into expression(s):"];
  printRRE[workingVarDerList];

  (* Make a list of dependent terms to supply to Coefficient. *)
  strippedTerms = Map[stripper, workingVarDerList];
  printRRE["At RRE 7, a list of stripped terms of the variational"<>
      " derivatives:"];
  printRRE[strippedTerms];

  (* Find any integers sitting alone in the stripped list and remove them. *)
  positionIntegers = Position[strippedTerms, _Integer, {0, 2}];
  printRRE["At RRE 8, the list of stripped terms has integers in positions:"];
  printRRE[positionIntegers];

  positionIntegers = Map[If[Length[Part[workingVarDerList, Part[#, 1]]] <= 1,
      Drop[#, -1], #] &, positionIntegers];
  printRRE["At RRE 9, after the position where integers occurred in the"<>
      " stripped list have been adjusted for lists of length 1:"];
  printRRE[positionIntegers];

  (* Identify and extract the terms associated with the integers in the *)
  (*  stripped list.                                                    *)
  loneConstants = Extract[workingVarDerList, positionIntegers];
  printRRE["At RRE 10, constants sitting alone in the variational"<>
      "derivatives:"];
  printRRE[loneConstants];

  (* Reduce the stripped terms list to one term of each kind.           *)
  strippedTerms = Union[Flatten[Delete[strippedTerms, positionIntegers]]];
  printRRE["At RRE 11, a condensed list of stripped terms:"];
  printRRE[strippedTerms];

  (* Group coefficients from the terms in the stripped list.            *)
  (* Coefficients containing dependent variables or independent         *)
  (* variables are repeats and must be removed.                         *)
  workingVarDerList = Map[Coefficient[workingVarDerList, #] &, strippedTerms];
  workingVarDerList = Flatten[Flatten[workingVarDerList] /.
      {u[_][__] -> 0, Derivative[__][u[_]][__] -> 0} /.
      {x -> 0, y -> 0, z -> 0, t -> 0}];
  printRRE["At RRE 12, a list of coefficients grouped using addition:"];
  printRRE[workingVarDerList];

  (* Add previously saved lone constants to the list of coefficients.   *)
  workingVarDerList = Union[loneConstants, workingVarDerList];

  workingVarDerList = Map[Equal[#, 0] &, workingVarDerList];
  printRRE["At RRE 13, grouped coefficients built into a system of"<>
      " equations:"];
  printRRE[workingVarDerList];

  coefficientSolns = Solve[workingVarDerList, constantList];
  printRRE["At RRE 14, solutions to the coefficient system:"];
  printRRE[coefficientSolns];

  (* Coefficients that fall on the right hand side are automatically    *)
  (* dependent.  Terms with these coefficients will be removed from the *)
  (* list of terms.                                                     *)
  revCoefficientSolns = coefficientSolns /. Rule[_, n1_] -> n1;
  nonZeroSolns = Union[Cases[revCoefficientSolns, q[_], {0, Infinity}]];
  printRRE["At RRE 15, the nonZeroSolutionList contains: ", nonZeroSolns];

  (* Check for other dependent terms whose coefficients may have been   *)
  (* on the left side of the equation.                                  *)
  revCoefficientSolns = DeleteCases[Flatten[coefficientSolns], Rule[_, 0]] /.
      Times[_, q[n1_]] -> q[n1] /. Plus -> List;
  printRRE["At RRE 16, All nonzero solutions are isolated and coefficients"<>
      " are removed from the terms:"];
  printRRE[revCoefficientSolns];

  dependentTerms = Union[revCoefficientSolns /. Rule[_, n1_] -> n1];
  printRRE["At RRE 17, all known dependent terms, grouped if necessary:"];
  printRRE[dependentTerms];

  otherDependents = Map[DeleteCases[revCoefficientSolns /.
      Rule -> rul, #, {0, Infinity}] &, dependentTerms];
  printRRE["At RRE 18, cases where two terms in rul[] may have a dependent"<>
      " coefficient:"];
  printRRE[otherDependents];

  otherDependents = Map[Cases[#, rul[_]] &, otherDependents] /. rul[n1_] -> n1;
  printRRE["At RRE 19, isolating possible dependent coefficients:"];
  printRRE[otherDependents];

  otherDependents = Flatten[Map[Drop[#, 1] &, otherDependents]];
  printRRE["At RRE 20, taking other dependent coefficients:"];
  printRRE[otherDependents];

  otherDependents = otherDependents/. q[n1_] -> {n1};
  nonZeroSolns = Union[otherDependents, nonZeroSolns] /. q[n1_] -> {n1};
  printRRE["At RRE 21, all divergence-equivalent terms are in positions:"];
  printRRE[nonZeroSolns];

  (* Begin removal of equivalent terms.                                 *)
  removedTerms = Extract[keepRhoTermsList, nonZeroSolns];
  printRRE["At RRE 22, the following terms are divergence equivalent to"<>
      " other terms in the list of density terms:"];
  printRRE[removedTerms];

  keepRhoTermsList = Delete[keepRhoTermsList, nonZeroSolns];
  printRRE["At RRE 23, this is the list of terms to be included in the"<>
      " candidate density:"];
  printRRE[keepRhoTermsList];

  (* Use Plus to reorder terms *)
  If[Length[keepRhoTermsList] > 1,
    keepRhoTermsList = Apply[Plus, keepRhoTermsList];
    keepRhoTermsList = Apply[List, keepRhoTermsList]
  ]; (* end If Length[keepRhoTerms] *)
  printRRE["At RRE 24, the list of terms on which to construct the density"<>
      " contains:"];
  printRRE[keepRhoTermsList];

  (* Clear all local variables not being returned.                      *)
  Clear[varDerivOnRhoTerms, positionOfDivergences, constantList, i1,
      workingVarDerList, strippedTerms, positionIntegers, loneConstants,
      coefficientSolns, revCoefficientSolns, nonZeroSolns, dependentTerms,
      n1, otherDependents, rul, removedTerms, printRRE];

  Return[keepRhoTermsList]
]; (* end Module reduceRhoEuler *)

(* ##########         Function: attachExplicitVars        ########## *)

(**********************************************************************)
(* attachExplicitVars[numericWtList, symbolicWtList, numDptVars,      *)
(*     indepVarsList, paramsWithWt, givenRank, explicitDeg]           *)
(* Purpose: To create a list of terms for the density including terms *)
(*          explicitly dependent on the independent variables.        *)
(* Input:   A list of numeric weights for dependent variables and     *)
(*              weighted parameters                                   *)
(*          A list of symbolic weights for dependent variables        *)
(*          The number of dependent variables as an integer           *)
(*          A list of independent variables                           *)
(*          A list of weighted parameters                             *)
(*          The rank for which the density is to be constructed       *)
(*          The highest degree for any explicit independent variables *)
(* Output:  Full list of terms for the candidate density              *)
(* Created: 6 April, 2007 at CSM                                      *)
(* Code is in File:  conservationlaws/nwatexvr.m                      *)
(* Last Modified:  19 May, 2008, 15:45 by DP at CSM                   *)
(**********************************************************************)

attachExplicitVars[numericWtList_, symbolicWtList_, numDptVars_,
    indepVarsList_,  paramsWithWt_, givenRank_, explicitDeg_] :=
Module[{indepVarsListWitht, varsComboAtDegree, ind, i1, i2, desiredDegree,
    n1, n2, listOfIndVarCombinations, newRules , fullWeightRules, variableWts,
    rule, simpleListOfWts, lst, diffTermsAtRank, listOfRhoTerms,
    derivativeWeightRule, m1, m2, m3, dvar, termWeightRule,
    weightedParametersRule, indepWeightRule, checkRank, printADV},

  If[debugAttachExplicitVars, printADV = Print, Clear[printADV],
      Clear[printADV]];

  printADV["debug code: ADV, Function: attachExplicitVars, File: nwatexvr.m"];

  printADV["At ADV IN, the numeric weight list is:"];
  printADV[numericWtList];
  printADV["At ADV IN, the symbolic weight list is:"];
  printADV[symbolicWtList];
  printADV["At ADV IN, the number of dependent variables is ", numDptVars];
  printADV["At ADV IN, the list of independent variables is ", indepVarsList];
  printADV["At ADV IN, the list of weighted parameters is ", paramsWithWt];
  printADV["At ADV IN, the rank for which the density is to be constructed"<>
      " is ", givenRank];
  printADV["At ADV IN, the highest allowable degree for explicit independent"<>
      " variables is ", explicitDeg];

  indepVarsListWitht = Union[indepVarsList, {t}];

  (* This function generates a list of combinations of the independent  *)
  (* variables for a given degree.                                      *)

  varsComboAtDegree[degree_, indVars_] := Flatten[Table[
      Apply[Times, Table[Part[indVars, ind[i1]], {i1, 1, degree - 1}]]*
      Drop[indVars, ind[degree - 1] - 1], {ind[1], 1, Length[indVars]},
      Evaluate[Apply[Sequence,
      Table[{ind[i2], ind[i2 - 1], Length[indVars]}, {i2, 2,degree - 1}]]]]];

  (* Get the highest degree for independent variables and rank to make  *)
  (* computations on.                                                   *)

  If[explicitDeg === Null,
    desiredDegree = Input["Enter a positive integer for the highest DEGREE"<>
        " for the independent variables.  Warning: Degrees higher than"<>
        " three may require a very long computation time."],
  (* else *)
    desiredDegree = explicitDeg
  ]; (* end If explicitDeg *)

  (* Compute the combinations of independent variables that can occur   *)
  (* for the given degree.                                              *)
  listOfIndVarCombinations = If[desiredDegree === 1,
      indepVarsListWitht,
      Union[Flatten[Table[varsComboAtDegree[i, indepVarsListWitht],
          {i, 2, desiredDegree}]], indepVarsListWitht]];
  printADV["At ADV 1, the list of independent variables to attach to"<>
      " differential terms:"];
  printADV[listOfIndVarCombinations];

  (* Set the weight rules for the independent variables and make a      *)
  (* weight list for all variables.                                     *)
  newRules = numericWtList /. Rule -> rule;
  newRules = Cases[newRules, rule[weight[d/_], _], {0, Infinity}];
  printADV["At ADV 2, weight rules for derivatives of independent variables"<>
      " have been singled out:"];
  printADV[newRules];

  newRules = newRules /. weight[d/n1_] -> weight[n1];
  newRules = newRules /. rule[n1_, n2_] -> rule[n1, -n2];
  newRules = newRules /. {dx -> x, dy -> y, dz -> z, dt -> t} /. rule -> Rule;
  printADV["At ADV 3, weight rules for independent variables:"];
  printADV[newRules];

  fullWeightRules = Union[numericWtList, newRules];
  printADV["At ADV 4, a list of all weight rules for the given PDE:"];
  printADV[fullWeightRules];

  (* Find the weights of the independent variable terms in combination  *)
  (* list.                                                              *)
  variableWts = PowerExpand[Map[Log, listOfIndVarCombinations]] /.
                Log -> weight;
  printADV["At ADV 5, the variable list changed to a weight list:"];
  printADV[variableWts];

  variableWts = variableWts /. newRules;
  printADV["At ADV 6, a list of weights corresponding to the terms in the"<>
      " independent variable combination list:"];
  printADV[variableWts];

  simpleListOfWts = givenRank - Union[variableWts];
  printADV["At ADV 7, a simple list of weights for determining what"<>
      " differential terms will be needed:"];
  printADV[simpleListOfWts];

  simpleListOfWts = DeleteCases[simpleListOfWts, n1_ /; n1 < 1];
  printADV["At ADV 8, a simple list of weights for determining what"<>
      " differential terms will be needed after negative weights have"<>
      " been removed:"];
  printADV[simpleListOfWts];

  variableWts = Thread[List[listOfIndVarCombinations, variableWts]];
  printADV["At ADV 9, a list of independent variable terms paired with"<>
           " their weights:"];
  printADV[variableWts];

  (* Adjust the rank on the terms according to the desired rank.  This  *)
  (* will show the rank of the differential terms that the independent  *)
  (* variable must be multiplied to in order to get the desired rank.   *)
  variableWts = Apply[lst, variableWts] /.
      List[n1_, n2_] -> List[n1, givenRank - n2];
  variableWts = DeleteCases[variableWts /. lst -> List, {_, n1_} /; n1 < 1];
  printADV["At ADV 10, a list of independent variable terms paired with"<>
      " their weights adjusted to meet the desired rank:"];
  printADV[variableWts];

  (* Generate differential terms at the needed ranks.  This will invoke *)
  (* buildRho at several ranks.                                         *)
  diffTermsAtRank[givenRank] = If[givenRank > 0,
      buildFullRho[numDptVars, paramsWithWt, numericWtList,
      symbolicWtList, indepVarsList, givenRank],
      {{}}];
  printADV["At ADV 11, the list of differential rho terms for"<>
          " rank ", givenRank, " is:"];
      printADV[diffTermsAtRank[givenRank]];
  Map[Set[diffTermsAtRank[#],
      buildFullRho[numDptVars, paramsWithWt, numericWtList, {{}},
                   indepVarsList, #]] &, simpleListOfWts];
  If[debugAttachExplicitVars,
    For[i1 = 1, i1 <= Length[simpleListOfWts], i1++,
      printADV["At ADV 12-", i1, ", the list of differential rho terms for"<>
          " rank ", Part[simpleListOfWts, i1], " is:"];
      printADV[diffTermsAtRank[Part[simpleListOfWts, i1]]];
    ] (* end For *)
  ]; (* end If debugAttachExplicitVars *)

  (* Formulate all possible terms for the density that have explicit    *)
  (* independent variables in them.                                     *)
  listOfRhoTerms = Map[Times[Part[#, 1], diffTermsAtRank[Part[#, 2]]] &,
      variableWts];
  printADV["At ADV 13, independent variable terms multiplied to appropriate"<>
      " differential terms:"];
  printADV[listOfRhoTerms];

  listOfRhoTerms = Flatten[Union[Flatten[listOfRhoTerms],
      diffTermsAtRank[givenRank]]];
  printADV["At ADV 14, a list of all terms to be considered for the"<>
      " candidate density:"];
  printADV[listOfRhoTerms];
  printADV["There are ", Length[listOfRhoTerms], " terms in this list."];

  If[debugAttachExplicitVars,
    derivativeWeightRule =
        {Derivative[m1_, _][u[dvar_]][__] -> m1*weight[d/dx] + weight[u[dvar]],
        Derivative[m1_, m2_, _][u[dvar_]][__] -> m1*weight[d/dx] +
            m2*weight[d/dy] + weight[u[dvar]],
        Derivative[m1_, m2_, m3_, _][u[dvar_]][__] -> m1*weight[d/dx] +
            m2*weight[d/dy] + m3*weight[d/dz] + weight[u[dvar]]};
    termWeightRule = {u[dvar_][__] -> weight[u[dvar]]};
    weightedParametersRule = Map[Rule[#, weight[#]] &, paramsWithWt];
    indepWeightRule = {x -> weight[x], y -> weight[y], z -> weight[z],
        t -> weight[t]};
    checkRank = PowerExpand[Map[Log, listOfRhoTerms]] /.
        derivativeWeightRule /. termWeightRule /. weightedParametersRule /.
        indepWeightRule /. Log[n1__] -> n1 /. fullWeightRules;
    printADV["At ADV 15, a list of ranks for each term in the final rho list"];
    printADV[checkRank]
  ]; (* end If debugAttachExplicitVars *)

  printADV["At ADV OUT, the list of terms for the candidate density:"];
  printADV[listOfRhoTerms];

  (* Clear all local variables not being returned.                      *)
  Clear[indepVarsListWitht, varsComboAtDegree, ind, i1, i2, desiredDegree, n1,
      n2, listOfIndVarCombinations, newRules , fullWeightRules, variableWts,
      rule, simpleListOfWts, lst, diffTermsAtRank, derivativeWeightRule, m1,
      m2, m3, dvar, termWeightRule, weightedParametersRule, indepWeightRule,
      checkRank, printADV];

  Return[{listOfRhoTerms}]
]; (* end Module attachExplicitVars *)

(* ##########     Mini-function: checkFormRhoAsDensity     ########## *)

(**********************************************************************)
(* checkFormRhoAsDensity[givenPDE, nameOfPDE, formRho, indepVars]     *)
(* Purpose: To check the form of a density given to the program by    *)
(*          the user.                                                 *)
(* Input:   The differential function from the given PDE              *)
(*          The name of the PDE                                       *)
(*          The form of density given by the user                     *)
(*          A list of independent variables                           *)
(* Output:  A printed response, nothing is returned                   *)
(* Created: 5 June, 2008 by DP  at CSM                                *)
(* Code is in File:  conservationlaws/nwevforh.m                      *)
(* Last Modified:  5 June, 2008, 8:36 by DP at home                   *)
(**********************************************************************)

checkFormRhoAsDensity[givenPDE_List, nameOfPDE_, formRho_, indepVars_List] :=
Module[{tderFormRho, evoRules, varDerivativeTder, i1, printCFRD},

  If[debugCheckFormRhoAsDensity, printCFRD = Print, Clear[printCFRD],
      Clear[printCFRD]];

  printCFRD["debug code: CFRD, Function: checkFormRhoAsDensity,"<>
      " File: nwevforh.m"];

  printCFRD["At CFRD IN, the given differential function(s) representing"<>
      " the PDE:"];
  printCFRD[givenPDE];
  printCFRD["At CFRD IN, the name of the PDE:"];
  printCFRD[nameOfPDE];
  printCFRD["At CFRD IN, the form of the density given by the user:"];
  printCFRD[formRho];
  printCFRD["At CFRD IN, the list of independent variables: ", indepVars];

  tderFormRho = D[formRho, t];
  printCFRD["At CFRD 1, the t-derivative of the form of the density:"];
  printCFRD[tderFormRho];

  evoRules = makeEvolutionRules[givenPDE];
  printCFRD["At CFRD 2, the evolution rule(s) returned are:"];
  printCFRD[evoRules];

  tderFormRho = replaceOnTheEquation[evoRules, tderFormRho, indepVars];
  printCFRD["At CFRD 3, the t-derivative of the density after all t-"<>
      "derivative have been replaced using the PDE:"];
  printCFRD[tderFormRho];

  varDerivativeTder = Map[Table[variationalDerivativeMultiD[#, i1, indepVars],
      {i1, 1, Length[givenPDE]}] &, tderFormRho];
  varDerivativeTder = Expand[Factor[varDerivativeTder]];
  printCFRD["At CFRD 4, the Euler operator applied to replaced t-derivative:"];
  printCFRD[varDerivativeTder];

  If[varDerivativeTder === Table[0, {i1, 1, Length[givenPDE]}],
    Print["The density given by the user is a density for the "<>
        nameOfPDE<>".  The program will analyze the density and"<>
       " report any simpler forms if they exist."],
  (* else *)
    Print["The density given by the user is not a density for the "<>
       nameOfPDE<>" in its current form.  The program will analyze the"<>
       " form given by the user attempt to find coefficients that will make"<>
       " this form a density."]
  ] (* end If varDerivativeTder *)
] (* end Module checkFormRhoAsDensity *)


(* ##########           Function: evaluateFormRho          ########## *)

(**********************************************************************)
(* evaluateFormRho[formOfRho, givenPDE, params, paramsWithWt,         *)
(*     indepVarsList]                                                 *)
(* Purpose: To take a user given density and reorganize the           *)
(*          information so that solveForCoefficientsWrapper can       *)
(*          analyze it.                                               *)
(* Input:   The form of density given by the user                     *)
(*          The differential function from the given PDE              *)
(*          A list of parameters that carry no weight                 *)
(*          A list of weighted parameters                             *)
(*          A list of independent variables                           *)
(* Output:  List of terms to be analyzed by the solver                *)
(* Created: 20 March, 2007 by DP  at CSM                              *)
(* Code is in File:  conservationlaws/nwevforh.m                      *)
(* Last Modified:  5 June, 2008, 8:51 by DP at home                   *)
(**********************************************************************)

evaluateFormRho[formOfRho_, givenPDE_, namePDE_, params_, paramsWithWt_,
    indepVarsList_] :=
Module[{checkDepVars, adjFormOfRho, i1, n1, n2, givenConstants, lengthRho,
    checkRho, ans, rhoTermsList, unknownTerms, unknownString, printEFR},

  If[debugEvaluateFormRho, printEFR = Print, Clear[printEFR], Clear[printEFR]];

  printEFR["debug code: EFR, Function: evaluateFormRho, File: nwevforh.m"];

  printEFR["At EFR IN, the expression given by the user with the form of"<>
      " the density. "];
  printEFR[formOfRho];
  printEFR["At EFR IN, the list of parameters given is: ", params];
  printEFR["At EFR IN, the list of weighted parameters given is: ",
      paramsWithWt];

  Print["A form for a density has been provided by the user.  The program"<>
      " will analyze the density, calcuate or recalculate coefficients"<>
      " and show the results."];

  Print["The density information given by the user is:"];
  Print[pdeform[formOfRho]];
  Print["------------------------------------------"];

  checkDepVars = Union[Cases[formOfRho, u[_][__], {0, Infinity}],
      Cases[formOfRho, Derivative[__][u[_]][__], {0, Infinity}]];
  If[checkDepVars === {},
    Print["The dependent variables have not been given in the correct form."<>
        " They need to be in the form u[1][x, t], u[2][x, t], with the"<>
        " head u."];
    Abort[]
  ]; (* end If checkDepVars *)

  (* Form of the density needs to be a sum of terms.                     *)
  adjFormOfRho = If[Head[formOfRho] === List, Apply[Plus, formOfRho],
      formOfRho, formOfRho];
  printEFR["At EFR 1, the form of the density given after applying Plus:"];
  printEFR[adjFormOfRho];

  (* Check the form of the density to see if it is an actual density or  *)
  (* if it may need some adjustment.                                     *)
  checkFormRhoAsDensity[givenPDE, namePDE, formOfRho, indepVarsList];
  Print["------------------------------------------"];

  (* Check user given density for undetermined constants. If the         *)
  (* constants in the correct form for c[i], leave alone.  Otherwise     *)
  (* inform the user that changes must be made.                          *)
  givenConstants = Cases[adjFormOfRho, _[n1_] /; Head[n1] === Integer,
      {0, Infinity}];
  givenConstants = Sort[givenConstants];
  printEFR["At EFR 2, unknown constants found in the user given density:"];
  printEFR[givenConstants];
  lengthRho = If[Head[adjFormOfRho] === List || Head[adjFormOfRho] === Plus,
      Length[adjFormOfRho], 1];
  checkRho = Table[c[i1], {i1, 1, lengthRho}];

  If[givenConstants === checkRho,
    printEFR["At EFR 3, constants given are legal, form given by user is"<>
        " returned to program untouched."];
    Return[adjFormOfRho],

  (* else *)
    (* Constants given by user by user either do not exist or are not   *)
    (* in the proper form.                                              *)
    If[givenConstants === {},
      Print["The program did not recognize any constants representing"<>
          " coefficients for the density terms."],

    (* else *)
      Print["The program found the constants"];
      Print[givenConstants];
      Print["  in the form of the density given by the user."]
    ]; (* end If givenConstants === {} *)

    Print["For the program to be able to evaluate"<>
        " this density, constants attached to each term must be in the"<>
        " form c[1], c[2], ..., c[n], numbered consecutively, and the"<>
        " head having the letter 'c'. The program"<>
        " can automatically rename the constants given, or the user can"<>
        " adjust the constants in the form of the density given in the"<>
        " input file.  If the user submits the constants (c[i]) in the"<>
        " correct form, the program will accept them in the user's format"<>
        " and run automatically."];
    ans = Input["The program does not find or is not able to use constants"<>
        " given by the user.  See the main notebook screen for details."<>
        " Do you wish to have the program automatically rename your"<>
        " constants?  (Yes or No).  If you say no, the program will quit"<>
        " immediately."];
    If[MemberQ[{y, yy, yes, YES, Yes, YEs, Y, YY, "y", "yes", "Y", "YES",
        "yy", "YY"}, ans],
      (* Put all terms into a list.                                     *)
      printEFR["At EFR 4, any given constants will be stripped off and"<>
          " density terms will be placed into a list so that the program"<>
          " can apply constants."];
      adjFormOfRho = Expand[adjFormOfRho];
      rhoTermsList = If[Head[adjFormOfRho] === Plus,
          Apply[List, adjFormOfRho], adjFormOfRho];
      rhoTermsList =
          If[Head[adjFormOfRho] =!= Plus && Head[adjFormOfRho] =!= List,
          {adjFormOfRho}, rhoTermsList, rhoTermsList];
      printEFR["At EFR 5, the terms of the user given density have been"<>
          " placed in a list:"];
      printEFR[rhoTermsList];
      (* First remove improper constants, then check for any other      *)
      (* possible constants that were not reported as parameters.       *)
      rhoTermsList = DeleteCases[rhoTermsList, n1_[n2_] /;
          n1 =!= u && n1 =!= List && Head[n2] === Integer, {0, Infinity}];
      printEFR["At EFR 6, all terms in the form _[_] have been removed:"];
      printEFR[rhoTermsList];

      unknownTerms = rhoTermsList /. {Plus -> Sequence, Times -> Sequence,
          Power -> Sequence};
      printEFR["At EFR 7, all rho terms broken into small pieces:"];
      printEFR[unknownTerms];

      unknownTerms = unknownTerms /. Flatten[{u[_][__] -> {},
          Derivative[__][u[_]][__] -> {}, Map[Rule[#, {}] &, paramsWithWt],
          Map[Rule[#, {}] &, params],
          Map[Rule[#, {}] &, Flatten[{indepVarsList, t}]]}];
      printEFR["At EFR 8, after removing all previously computed pieces:"];
      printEFR[unknownTerms];

      unknownTerms = Union[Cases[unknownTerms, _Symbol, {0, Infinity}]];
      printEFR["At EFR 9, a list of undetermined constants:"];
      printEFR[unknownTerms];

      (* Check for undetermined constants and/or unweighted parameters   *)
      (* given in the user density.  They may interfere with calculation *)
      (* of the c[i].                                                    *)
      If[unknownTerms =!= {},
        Print[If[Intersection[unknownTerms, params] === params &&
            params =!= {}, "Parameters ",
            If[Intersection[unknownTerms, params] =!= {}, "Parameters and"<>
                " undetermined constants ", "Undetermined constants "]]];
        Print[unknownTerms];
        Print["  have been found in the user given density.  The program"<>
            " will automatically consider parameters and constants given in"<>
            " the PDE when calculating the coefficients for the terms in"<>
            " the density.  The program may not be able to calculate the"<>
            " density with these terms present."];

        unknownString = Map[ToString, unknownTerms];
        For[i1 = 1, i1 <= Length[unknownTerms], i1++,
          ans = Input["See the information on the main notebook screen"<>
              " about undetermined constants found in the user density."<>
              " Do you wish to keep " <> Part[unknownString, i1] <>
              " in the calculations? (Yes or No)"];
          If[MemberQ[{n, nn, no, NO, No, N,NN, "n", "no", "N", "No",
              "nn", "NN", "NO"}, ans],
            rhoTermsList =
                DeleteCases[rhoTermsList, Part[unknownTerms, i1], {0, 2}];
            printEFR["At EFR 9, ", Part[unknownTerms, 1], " has been removed"<>
                " from any terms in the density that previously contained"<>
                " it.  The terms in the density are now:"];
            printEFR[rhoTermsList];

            Print[Part[unknownTerms, i1], " has been removed from user"<>
                " density."];
          ] (* end If ans === No *)
        ]; (* end For *)

        Print["The density"];
        Print[Apply[Plus, rhoTermsList]];
        Print["  will be submitted to the program.  Coefficient constants"<>
            " will be reassigned (shown in the next step) and calculated. "<>
            " The density and its associated flux will appear when the"<>
            " program ends."];
        Print["------------------------------------------"]
      ], (* end If unknownTerms *)

    (* else *)
      Print["All computations are being discontinued."];
      Abort[]
    ] (* end If ans === Yes *)
  ]; (* end If givenConstants *)

  (* Clear all local variables not being returned.                      *)
  Clear[checkDepVars, adjFormOfRho, i1, n1, givenConstants, lengthRho,
      checkRho, ans, unknownTerms, unknownString, printEFR];

  Return[{rhoTermsList}]
]; (* end Module evaluateFormRho *)

(* ##########        Mini-function: threadConstants        ########## *)

(**********************************************************************)
(* threadConstants[rhoTerms]                                          *)
(* Purpose: To place unknown coefficients on each of the rho terms    *)
(* Input:   List of terms for the candidate density                   *)
(* Output:  expression with coefficients of c[i]                      *)
(* Code is in File:  conservationlaws/dpsocowr.m                      *)
(* Last Modified:                                                     *)
(**********************************************************************)

(* threadConstants places unknown coefficients on each of the rho terms. *)
threadConstants[rhoTerms_List] := Module[{rhoWithConstants, i1},

  If[debugThreadConstants, printTC = Print, Clear[printTC], Clear[printTC]];

  printTC["debug code: TC, Function: threadConstants, File: dpsocowr.m"];

  rhoWithConstants = Apply[Plus, Thread[Times[rhoTerms, Table[c[i1],
      {i1, 1, Length[rhoTerms]}]]]];
  printTC["At TC 1, rho as an expression with constants:"];
  printTC[rhoWithConstants];

  Return[rhoWithConstants]
]; (* end Module threadConstants *)

(* ##########    Function: solveForCoefficientsWrapper     ########## *)

(**********************************************************************)
(* solveForCoefficientsWrapper[diffFunctionList, rhoIn, rank,         *)
(*     numDepVars, indepVarList, weightParameters, noWeightParameters]*)
(* Purpose: To solve for coefficients and generate the final rhos     *)
(* Input:   List of differential functions                            *)
(*          List of terms for each density that can be formed         *)
(*          The rank of the density                                   *)
(*          Number of dependent variables                             *)
(*          List of independent variables                             *)
(*          List of weighted parameters                               *)
(*          List of non weighted parameters                           *)
(* Output:  Densities in final form with undetermined coefficients    *)
(*              calculated                                            *)
(*          A list of evolution rules for the PDE                     *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whsocowr.m, Created 2 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpsocowr.m                      *)
(* Major Changes Made: 14 June, 2006, 6:10 by DP at home              *)
(* Last Modified:  13 October, 2009, 10:44 by DP at CSM               *)
(**********************************************************************)

solveForCoefficientsWrapper[diffFunctionList_, rhoIn_, rank_, numDepVars_,
    indepVarList_List, weightParameters_List, noWeightParameters_List] :=
Module[{parameters, evolutionRules, rhoWithConstants, dtRho, numDtRhoTerms,
    termsInRhoList, i1, systemOfCoefficients, grcs, finalListOfRhos = {},
    gs, rhoOut, printSFCW},

  If[debugSolveWrapper, printSFCW = Print, Clear[printSFCW], Clear[printSFCW]];

  printSFCW["debug code: SFCW, Function: solveForCoefficientsWrapper,"<>
      " File: dpsocowr.m"];

  printSFCW["Solving for coefficients in solveForCoefficinetsWrapper."];
  printSFCW["At SFCW IN, the differential function list consists of:"];
  printSFCW[diffFunctionList];
  printSFCW["At SFCW IN, the rank of the candidate(s) is ", rank];
  printSFCW["At SFCW IN, the list of rho candidate terms consists of"];
  printSFCW[rhoIn];
  If[Length[rhoIn===1],
    printSFCW["At SFCW IN, there is one rho candidate"],
    printSFCW["At SFCW IN, there are ", Length[rhoIn], " rho candidates"]];
  printSFCW["At SFCW IN, the number of dependent variable(s) is ",
      numDepVars];
  printSFCW["At SFCW IN, the independent variable list contains ",
      indepVarList];
  printSFCW["At SFCW IN, the weighted parameter list contains: ",
      weightParameters];
  printSFCW["At SFCW IN, the no weight parameter list contains: ",
      noWeightParameters];

  parameters = Union[weightParameters, noWeightParameters];

  (* Make evolution rules to replace on the equation.                   *)
  evolutionRules = makeEvolutionRules[diffFunctionList];
  printSFCW["At SFCW 1 evolutionRules:"];
  printSFCW[evolutionRules];

  (* Place constants on each term of rho.                               *)
  (* Note: Head can be Plus only when a user density is submitted in    *)
  (* input file.                                                        *)
  If[Head[rhoIn] =!= List,
    rhoWithConstants = {rhoIn},
  (* else *)
    rhoWithConstants = Map[threadConstants, rhoIn]
  ]; (* end If Head *)

  printSFCW["At SFCW 2, result after placing constants on the rhos:"];
  printSFCW[rhoWithConstants];
  printSFCW["At SFCW 2a, there are ", Length[rhoWithConstants],
      " densities in this list."];

  (* Print each bucket showing the rho expressions with constants.      *)
  printRho[rhoWithConstants, "CANDIDATES for DENSITIES"];

  (* Find the t-derivatives of each rho given in the input list.        *)
  dtRho = Map[D[#, t] &, rhoWithConstants];
  printSFCW["At SFCW 3, applying the t-derivative to each rho gives:"];
  printSFCW[dtRho];
  printSFCW["At SFCW 3a, the number of t-derivative(s) of rho is ",
      Length[dtRho]];

  (* Replace t-derivatives using evolution rules.                       *)
  numDtRhoTerms = Length[dtRho];
  dtRho = Map[replaceOnTheEquation[evolutionRules, #, indepVarList] &,
      dtRho];
  printSFCW["At SFCW 4, dtRho after t-derivatives have been replaced"<>
      " using the evolution rules:"];
  printSFCW[dtRho];
  printSFCW["At SFCW 4a, dtRho has ", numDtRhoTerms, " expression"<>
      If[numDtRhoTerms === 1, ".", "s."]];

  termsInRhoList = Map[Length, rhoWithConstants, {1}];
  printSFCW["At SFCW 5, the longest candidate density"<>
      " has ", Max[termsInRhoList], " terms, and the total number of terms"<>
      " in all candidates is ", Apply[Plus, termsInRhoList], "."];
  If[Max[termsInRhoList] < 250 && Apply[Plus, termsInRhoList] < 1500,
    (* Extract the system of coefficients.                               *)
    systemOfCoefficients = Thread[grcs[dtRho, rhoWithConstants,
        Table[numDepVars, {i, 1, numDtRhoTerms}],
        Table[indepVarList, {i, 1, numDtRhoTerms}],
        Table[weightParameters, {i, 1, numDtRhoTerms}]]]
         /. grcs -> getRhoCoefficientsSystem;

    For[i1 = 1, i1 <= Length[systemOfCoefficients], i1++,
      printSFCW["At SFCW 6, a system of coefficients for rho ",  i1, " is"];
      printSFCW[Map[Equal[#, 0] &, Part[systemOfCoefficients, i1, 1]]];
      printSFCW["At SFCW 7, the undetermined coefficients are:"];
      printSFCW[Part[systemOfCoefficients, i1, 2]];
    ]; (* end for *)
    printSFCW["At SFCW 8, there are ", Length[systemOfCoefficients], ""<>
        " systems in this list:"];
    printSFCW[systemOfCoefficients];

    (* Solve the system of coefficients and state the final form for     *)
    (* the densities.                                                    *)
    globalKcounter = 1;
    finalListOfRhos = Thread[gs[systemOfCoefficients, rhoWithConstants,
        Table[parameters, {i1, 1, numDtRhoTerms}],
        Table[weightParameters, {i1, 1, numDtRhoTerms}],
        Table[numDepVars, {i1, 1, numDtRhoTerms}],
        Table[indepVarList, {i1, 1, numDtRhoTerms}],
        Table[rank, {i1, 1, numDtRhoTerms}]]] /. gs -> generalSolver;
    finalListOfRhos = Flatten[DeleteCases[finalListOfRhos, {{}, {}}], 1];
    printSFCW["At SFCW 9, the list of expressions coming out of the general"<>
        " solver:"];
    printSFCW[finalListOfRhos],

  (* else *)
    printSFCW["At SFCW 10, entering alternate route for solving system -"<>
        " evaluating one system at a time."];
    For[i1 = 1, i1 <= numDtRhoTerms, i1++,

      (* Extract the system of coefficients, one system at a time.      *)
      systemOfCoefficients = getRhoCoefficientsSystem[Part[dtRho, i1],
          Part[rhoWithConstants, i1], numDepVars, indepVarList,
          weightParameters];
      printSFCW["At SFCW 11, a system of coefficients for rho ",  i1, " is"];
      printSFCW[Map[Equal[#, 0] &, Part[systemOfCoefficients, 1]]];
      printSFCW["At SFCW 12, the undetermined coefficients are:"];
      printSFCW[Part[systemOfCoefficients, 2]];

      (* Solve the system of coefficients and state the final form for   *)
      (* the densities.                                                  *)
      globalKcounter = i1;
      rhoOut = generalSolver[systemOfCoefficients, Part[rhoWithConstants, i1],
          parameters, weightParameters, numDepVars, indepVarList, rank];
      finalListOfRhos = If[rhoOut =!= {{}, {}},
          Union[finalListOfRhos, rhoOut], finalListOfRhos];
      printSFCW["At SFCW 13, the list of expressions after loop ", i1, ""<>
          " through the general solver:"];
      printSFCW[finalListOfRhos]
    ] (* end For *)
  ]; (* end If Max[termsInRhoList] *)

  (* Eliminate any duplicate density/compatibility condition pairs.     *)
  finalListOfRhos = Union[finalListOfRhos];
  printSFCW["At SFCW 14, All duplicate density/compatibility condition"<>
      " pairs have been removed from the rho list:"];
  printSFCW[finalListOfRhos];

  If[debugSolveWrapper,
    For[i1 = 1, i1 <= Length[finalListOfRhos], i1++,
      printSFCW["At SFCW 15, density no. ", i1, " for the given PDE and "<>
          " given rank is:"];
      printSFCW[Part[finalListOfRhos, i1, 1]];
      printSFCW["At SFCW 15a, compatibility conditions on this density:"];
      printSFCW[If[Part[finalListOfRhos, i1, 2] === {},
          "None", Part[finalListOfRhos, i1, 2]]];
    ] (* end For *)
  ]; (* end If debug *)

  (* If a nonzero density exists, remove all zero densities.            *)
  If[finalListOfRhos =!= {{0, {}}},
    finalListOfRhos = DeleteCases[finalListOfRhos, {0, {}}]];
  printSFCW["At SFCW 16, list of densities paired with compatibility"<>
      " conditions after eliminating zero densities:"];
  printSFCW[finalListOfRhos];

  printSFCW["At SFCW OUT, the final list of densities:"];
  printSFCW[finalListOfRhos];
  printSFCW["At SFCW OUT, the evolution rules are:"];
  printSFCW[evolutionRules];

  (* Clear all local variables not being returned.                      *)
  Clear[parameters, rhoWithConstants, dtRho, numDtRhoTerms, termsInRhoList,
      i1, systemOfCoefficients, grcs, gs, rhoOut, printSFCW];

  Return[{finalListOfRhos, evolutionRules}]
]; (* end Module solveForCoefficientsWrapper *)

(* ##########        Function: makeEvolutionRules          ########## *)

(**********************************************************************)
(* makeEvolutionRules[diffFunctionList]                               *)
(* Purpose: To convert the evolution equations given in the data file *)
(*          to rules that can be used to substitute on the equation   *)
(* Input:   A differential Function List                              *)
(* Output:  A list of evolution substitution rules                    *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whmevoru.m, Created 2 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpmevoru.m                      *)
(* Last Modified:  29 April, 2008, 16:23 by DP at CSM                 *)
(**********************************************************************)

makeEvolutionRules[diffFunctionList_] :=
Module[{tDerivativeTerms, i1, m1, n1, eqnsWithoutTDerivs, tDerivativeCoefs,
    evolutionRules, printMER},

  If[debugEvolutionRules, printMER = Print, Clear[printMER], Clear[printMER]];

  printMER["debug code: MER, Function: makeEvolutionRules, File: dpmevoru.m"];

  printMER["At MER IN, the list of differential functions given:"];
  printMER[diffFunctionList];

  (* Find the terms with the time derivatives.                          *)
  tDerivativeTerms = Table[Cases[Expand[diffFunctionList[[i1]]],
      Derivative[__, n1_][u[m1_]][__] |
      Times[___, Derivative[__, n1_][u[m1_]][__]] /; n1 === 1, {0, 1}],
      {i1, 1, Length[diffFunctionList]}];
  printMER["At MER 1, each inner list is a list of time derivatives"<>
      " for each given differential function:"];
  printMER[tDerivativeTerms];

  (* Check that the equations are indeed evolution equations.           *)
  If[Position[Map[Length, tDerivativeTerms],
    _?(#1 =!= 1 && Head[#1] === Integer &)] =!= {},
    Print["Equations in data file are not in evolution form"];
    Print["All computations are being discontinued."];
    Abort[],
  (* else *)
    printMER["At MER 2, equations are in evolution form."]
  ]; (* end If Position *)

  (* Remove the t-derivative terms from the equations.                  *)
  eqnsWithoutTDerivs = diffFunctionList /.
      Map[Rule[#, 0]&, Flatten[tDerivativeTerms]];
  printMER["At MER 3, differential functions without the t-derivative"<>
      " terms;"];
  printMER[eqnsWithoutTDerivs];

  (* Strip any coefficients from the t-derivative terms.                 *)
  tDerivativeCoefs = tDerivativeTerms /. Derivative[__][u[_]][__] -> 1;
  printMER["At MER 4, coefficients on the t-derivative terms:"];
  printMER[tDerivativeCoefs];

  (* Solve each equation for the t-derivative term.                     *)
  eqnsWithoutTDerivs = Thread[Times[-1/tDerivativeCoefs, eqnsWithoutTDerivs]];
  printMER["At MER 5, differential functions adjusted for coefficients:"];
  printMER[eqnsWithoutTDerivs];

  tDerivativeTerms = Thread[Times[1/tDerivativeCoefs, tDerivativeTerms]];
  printMER["At MER 6, t-derivative terms adjusted for coefficients:"];
  printMER[tDerivativeTerms];

  (* Set up the evolution rules.                                       *)
  evolutionRules = Thread[Rule[Flatten[tDerivativeTerms],
      Flatten[eqnsWithoutTDerivs]]];
  printMER["At MER 7, evolution rules: t-derivatives replaced by their"<>
      " space equivalents:"];
  printMER[evolutionRules];

  (* Clear all local variables not being returned.                      *)
  Clear[tDerivativeTerms, i1, m1, n1, eqnsWithoutTDerivs, tDerivativeCoefs,
    printMER];

  Return[Expand[evolutionRules]]
]; (* end Module makeEvolutionRules *)

(* ##########       Function: replaceOnTheEquation         ########## *)

(**********************************************************************)
(* replaceOnTheEquation[rules, dtRho, indepVarList]                   *)
(* Purpose: To replace all t-derivatives in D_t(rho) with their       *)
(*          evolution equivalents from the given PDE                  *)
(* Input:   A list of evolution rules based on the PDE                *)
(*          The total t-derivative of a density                       *)
(*          A list of independent variables                           *)
(* Output:  The D_t(rho) with no t-derivatives                        *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whrepoeq.m, Created 4 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dprepoeq.m                      *)
(* Major Changes Made: 11 June, 2006, 6:25 by DP at home              *)
(* Major Changes Made: 17 August, 2006, 11:00 by DP at home           *)
(* Major Changes Made: 5 February, 2008, 17:00 by DP at CSM           *)
(* Last Modified:  29 April, 2008, 16:37 by DP at CSM                 *)
(**********************************************************************)

replaceOnTheEquation[evolutionRules_List, tDerOfDensity_, indepVar_List] :=
Module[{lthIndVar = Length[indepVar], derPattern1, derPattern2, n1, n2, n3,
    n4, n5, revisedEvoRules, replaceRule, replacedDtDensity, ddd, printROTE},

  If[debugReplaceOnTheEquation, printROTE = Print, Clear[printROTE],
      Clear[printROTE]];

  printROTE["debug code: ROTE, Function: replaceOnTheEquation,"<>
      " File: dprepoeq.m"];

  printROTE["At ROTE IN, the total t-derivative of the density:"];
  printROTE[tDerOfDensity];
  printROTE["At ROTE IN, the list of evolution rules contains:"];
  printROTE[evolutionRules];
  printROTE["At ROTE IN, the list of independent variables is: ", indepVar];

  (* Build pattern matching forms.                                      *)
  derPattern1 = If[lthIndVar === 1, {n1_, 1},
    If[lthIndVar === 2, {n1_, n2_, 1}, {n1_, n2_, n3_, 1}]];
  derPattern2 = If[lthIndVar === 1, {{x, n1}},
    If[lthIndVar ===  2, {{x, n1}, {y, n2}}, {{x, n1}, {y, n2}, {z, n3}}]];
  printROTE["At ROTE 1, the patterns for remaking the derivatives during"<>
      " the replacement process:"];
  printROTE["Pattern1 = ", derPattern1, "\nPattern2 = ", derPattern2];

  (* Extract the dependent variable number from the evolution rule.     *)
  revisedEvoRules = evolutionRules /.
    Rule[Derivative[__, 1][u[n4_]][__], n5__] -> List[n4, n5];
  printROTE["At ROTE 2, the evolution rules have been revised so that the"<>
      " number for the dependent variable makes up the left-hand side of"<>
      " the rule:"];
  printROTE[revisedEvoRules];

  (* Set up the general rule for the replacement. *)
  replaceRule = Map[
      Derivative[Apply[Sequence, derPattern1]][u[Part[#, 1]]][__] ->
      ddd[Part[#, 2], Apply[Sequence, derPattern2]] &, revisedEvoRules];
  printROTE["At ROTE 3, the general replacement rule(s) incorporating the"<>
      " evolution rules:"];
  printROTE[replaceRule];

  replacedDtDensity = tDerOfDensity /. replaceRule /. ddd -> D;
  printROTE["At ROTE 4, the t-derivative of the density after the"<>
      " replacement rules have been applied:"];
  printROTE[replacedDtDensity];

  (* Clear all local variables not being returned.                      *)
  Clear[lthIndVar, derPattern1, derPattern2, n1, n2, n3, n4, n5,
      revisedEvoRules, replaceRule, printROTE];

  Return[replacedDtDensity]
](* end Module replaceOnTheEquation *)

(* ##########     Function: getRhoCoefficientsSystem       ########## *)

(**********************************************************************)
(* getRhoCoefficientsSystem[dtRhoCandidate, rhoCandidateTerm,         *)
(*       numDepVars, indepVarList, parameterList]                     *)
(* Purpose: To set up a system of equations in order to solve for     *)
(*          the undetermined constants, c[i].                         *)
(* Input:   The t-derivative of the current density                   *)
(*          The current density                                       *)
(*          The number of Dependent Variables                         *)
(*          An independent Variable List                              *)
(*          A list of Weighted Parameters                             *)
(* Output:  A system of equations involving coefficients and the      *)
(*              undetermined coefficients, c[i]                       *)
(*          List of undetermined coefficients, c[i]                   *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whsorhoc.m, Created 9 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpgrhocs.m                      *)
(* Last Modified:  19 May, 2008, 15:14 by DP at CSM                   *)
(**********************************************************************)

getRhoCoefficientsSystem[dtRhoCandidate_, rhoCandidateTerm_, numDepVars_,
    indepVarList_, parameterList_] :=
Module[{unknownCoefList, currentDepVar = numDepVars, newtdtRhoCandidate,
    knownZerocList = {}, variationalDerComponent, coefEqnList,
    fullCoefEqnList = {}, printGRCS},

  If[debugGetRhoCoefficientsSystem, printGRCS = Print, Clear[printGRCS],
      Clear[printGRCS]];

  printGRCS["debug code: GRCS, Function: getRhoCoefficientsSystem,"<>
      " File: dpgrhocs.m"];

  printGRCS["At GRCS IN, the current density being evaluated:"];
  printGRCS[rhoCandidateTerm];
  printGRCS["At GRCS IN, the total t-derivative of the current density"<>
      " being evaluated:"];
  printGRCS[dtRhoCandidate];
  printGRCS["At GRCS IN, the number of dependent variables is ", numDepVars];
  printGRCS["At GRCS IN, the independent variable list is: ", indepVarList];
  printGRCS["At GRCS IN, the parameter list (weighted parameters only)"<>
      " contains: ", parameterList];

  (* Make a list of the coefficients to be solved for.                   *)
  unknownCoefList = Union[Cases[rhoCandidateTerm, c[_], {0, Infinity}]];
  printGRCS["At GRCS 1, a list of undetermined coefficients:"];
  printGRCS[unknownCoefList];

  (* Work on one component of the variational derivative at a time,     *)
  (* simultaneously obtaining the coefficient system and reducing the   *)
  (* system by removing all terms with coefficients that can be         *)
  (* found to be zero.                                                  *)
  While[currentDepVar > 0,
    (* Simplify the tder list by replacing coefficients know to be zero *)
    (* with zero.                                                       *)
    newtdtRhoCandidate = dtRhoCandidate /. knownZerocList;
    printGRCS["At GRCS 2, D_t (rho) with zero coefficients replaced with"<>
        " zero:"];
    printGRCS[newtdtRhoCandidate];
    printGRCS["At GRCS 2a, D_t (rho) has ",
        Length[Expand[newtdtRhoCandidate]], " terms."];

    (* Apply the variational derivative to D_t rho.                     *)
    variationalDerComponent = variationalDerivativeMultiD[newtdtRhoCandidate,
        currentDepVar, indepVarList];
    printGRCS["At GRCS 3, the variational derivative for the component"<>
        " for u["<>ToString[currentDepVar]<>"] on D_t (rho):"];
    printGRCS[variationalDerComponent];
    printGRCS["At GRCS 3a, the variational derivative has ",
        Length[Expand[variationalDerComponent]], " terms."];

    (* Make a list of equations consisting of coefficients only.           *)
    {coefEqnList, knownZerocList} = extractCoefficientSystem[
        variationalDerComponent, unknownCoefList, parameterList,
        knownZerocList];
    printGRCS["At GRCS 4, the system of coefficient expressions returned"<>
        " from extractCoefficientSystem:"];
    printGRCS[coefEqnList];
    printGRCS["At GRCS 5, a list of c[i] known to be zero:"];
    printGRCS[knownZerocList];

    fullCoefEqnList = Union[fullCoefEqnList, coefEqnList];
    printGRCS["At GRCS 6, the full system of coefficient expressions for"<>
        " variational derivatives evaluated so far:"];
    printGRCS[fullCoefEqnList];

    fullCoefEqnList = fullCoefEqnList /. knownZerocList;
    printGRCS["At GRCS 7, the full system of coefficient expressions"<>
        " after applying the updated zero coefficient rules:"];
    printGRCS[fullCoefEqnList];

    fullCoefEqnList = DeleteCases[Flatten[fullCoefEqnList], m1_ /; m1 === 0];
    printGRCS["At GRCS 8, the full system of coefficient expressions"<>
        " after removing any zero expressions:"];
    printGRCS[fullCoefEqnList];

    currentDepVar--
  ]; (* end While *)

  (* Clear all local variables not being returned.                      *)
  Clear[currentDepVar, newtdtRhoCandidate, variationalDerComponent,
      coefEqnList, printGRCS];

  Return[{fullCoefEqnList, unknownCoefList, knownZerocList}]
]; (* end Module getRhoCoefficientsSystem *)

(* ##########      Function: extractCoefficientSystem      ########## *)

(**********************************************************************)
(* extractCoefficientSystem[expressionIn, unknownCoefList,            *)
(*     parameters, cKnownToBeZero]                                    *)
(* Purpose: To extract a linear system to solve for the coefficients  *)
(* Input:   An expression in Euler form containing coefficients c[i]  *)
(*          A list of all undetermined coefficients, c[i]             *)
(*          A list of all parameters in the problem                   *)
(*          A list of undetermined coefficients already determined to *)
(*              be zero                                               *)
(* Output:  A system of coefficient expressions formed from the given *)
(*              expression                                            *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whexsyco.m, Created 7 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/nwexcosy.m                      *)
(* Major Changes Made: 13 September, 2007, by DP at CSM.   Revisions  *)
(*      have completely changed this function from its original form. *)
(* Last Modified:  19 June, 2008, 17:50 by DP at CSM                  *)
(**********************************************************************)

extractCoefficientSystem[expressionIn_, unknownCoefList_, parameters_,
    cKnownToBeZero_] :=
Module[{expdExpnIn, conHead = Head[Part[unknownCoefList, 1]], loneTerms,
    workingVarDerList, termsToRemove, coefficientExpns, i1, n1, term,
    posSingleTerms, zerocRules, cKnownToBeZeroUpdate, recipTermsToRemove,
    times, printECS},

  If[debugExtractCoefficientSystem, printECS = Print, Clear[printECS],
      Clear[printECS]];

  printECS["debug code: ESFC, Function: extractSystemForCoefficients,"<>
      " File: nwexcosy.m"];

  printECS["At ECS IN, the given expression with undetermined"<>
      " coefficients:"];
  printECS[expressionIn];
  printECS["At ECS IN, a list of undetermined coefficients found in the"<>
      " expression:"];
  printECS[unknownCoefList];
  printECS["At ECS IN, a list of parameters given: ", parameters];
  printECS["At ECS IN, undetermined coefficients known to be zero:"];
  printECS[cKnownToBeZero];
  printECS["At ECS IN, the head on the unknown constant is ", conHead];

  If[expressionIn === 0, Return[{{}, cKnownToBeZero}]];

  expdExpnIn = Expand[expressionIn];
  printECS["At ECS 1, the given expression in expanded form:"];
  printECS[expdExpnIn];

  (* Find the positions of the terms that have c[i] sitting alone.      *)
  loneTerms = Cases[expdExpnIn + term, _conHead | Times[n1__, conHead[_]] /;
      Not[MemberQ[{n1}, Alternatives[u[_][__], Derivative[__][_][__],
      x, y, z, t], {0, Infinity}]], {0, 1}];
  printECS["At ECS 2, all ", conHead, "[i] not connected to"<>
      " u[_] or its derivatives or any independent variables:"];
  printECS[loneTerms];

  (* Remove all lone c[i].                                              *)
  workingVarDerList = expdExpnIn - Apply[Plus, loneTerms];
  printECS["At ECS 3, the variational derivative list after lone"<>
      " constants are removed:"];
  printECS[workingVarDerList];

  (* Make a list of terms needed to identify common coefficients.       *)
  termsToRemove = Union[stripper[workingVarDerList, parameters]];
  printECS["At ECS 4, stripper is used to identify terms used as a basis"<>
      " for forming coefficient equations:"];
  printECS[termsToRemove];

  (* Match all terms in the given expression according to the terms     *)
  (* given in the previous list.                                        *)
  coefficientExpns = Table[Cases[workingVarDerList,
      Times[n1__, Part[termsToRemove, i1]] /;
      Not[MemberQ[{n1}, Alternatives[u[_][__], Derivative[__][u[_]][__],
      x, y, z, t], {0, Infinity}]], {0, 1}], {i1, 1, Length[termsToRemove]}];
  printECS["At ECS 5, the expression(s) from the common terms list after"<>
      " they have been matched to the stripped terms:"];
  printECS[coefficientExpns];

  If[Flatten[coefficientExpns] =!= {},
    posSingleTerms = Position[coefficientExpns, _?(Length[#] === 1 &), {0, 1}];
    printECS["At ECS 6, the positions of all single terms in the expression"<>
        " list (All "<>ToString[conHead]<>"[i] in these terms must be zero):"];
    printECS[posSingleTerms];

    If[posSingleTerms =!= {{}},
      zerocRules = Union[Cases[Extract[coefficientExpns, posSingleTerms],
          conHead[_], {0, Infinity}]];
      printECS["At ECS 7, undetermined coefficients that must be zero"<>
          " because they exist in expressions with single terms:"];
      printECS[zerocRules];

      coefficientExpns = Delete[coefficientExpns, posSingleTerms];
      coefficientExpns = If[coefficientExpns === Null, {}, coefficientExpns];
      printECS["At ECS 8, the expression list after single terms have been"<>
          " removed:"];
      printECS[coefficientExpns];

      termsToRemove = Delete[termsToRemove, posSingleTerms];
      termsToRemove = If[termsToRemove === Null, {}, termsToRemove];
      printECS["At ECS 9, the stripped terms list after single terms have"<>
          " been removed:"];
      printECS[termsToRemove];

    zerocRules = Map[Rule[#, 0] &, zerocRules];
    printECS["At ECS 10, rules for zero undetermined coefficients:"];
    printECS[zerocRules],

    (* else *)
      zerocRules = {}
    ]; (* end If posSingleTerms *)

    cKnownToBeZeroUpdate = Union[cKnownToBeZero, zerocRules];
    printECS["At ECS 11, all zero rules for undetermined coefficients"<>
        " in the candidate density:"];
    printECS[cKnownToBeZeroUpdate];

    (* Remove all parts that are not coefficients.                       *)
    recipTermsToRemove = Map[Power[#, -1] &, termsToRemove];
    printECS["At ECS 12, the reciprocal to be applied to the terms that"<>
        " must be removed list:"];
    printECS[recipTermsToRemove];

    coefficientExpns = Thread[times[coefficientExpns, recipTermsToRemove]] /.
        times -> Times;
    coefficientExpns = Union[coefficientExpns /. cKnownToBeZeroUpdate];
    printECS["At ECS 13, lists of terms that will form each coefficient"<>
        " expression:"];
    printECS[coefficientExpns];

    coefficientExpns = Apply[Plus, coefficientExpns, {1}];
    printECS["At ECS 14, Plus has been applied to each group of terms:"];
    printECS[coefficientExpns],

  (* else *)
    cKnownToBeZeroUpdate = cKnownToBeZero
  ]; (* end If coefficientExpns *)

  (* Put in expressions formed using lone constants.                    *)
  loneTerms = Apply[Plus, loneTerms];
  printECS["At ECS 15, lone constants organized into a coefficient"<>
      " expression:"];
  printECS[loneTerms];

  coefficientExpns = Union[coefficientExpns, {loneTerms}];
  coefficientExpns = Flatten[DeleteCases[coefficientExpns, 0, {1}]];
  printECS["At ECS 16, lone constant expressions have been added to the"<>
      " list of coefficient equations:"];
  printECS[coefficientExpns];

  printECS["At ECS OUT, the system of coefficient expressions contains:"];
  printECS[coefficientExpns];

  (* Clear all local variables not being returned.                      *)
  Clear[expdExpnIn, conHead, loneTerms, workingVarDerList, termsToRemove, i1,
      n1, posSingleTerms, zerocRules, recipTermsToRemove, times, printECS];

  Return[{coefficientExpns, cKnownToBeZeroUpdate}]
]; (* end Module extractSystemForCoefficients *)

(* ##########    Function: extractSystemForCoefficients    ########## *)

(**********************************************************************)
(* extractSystemForCoefficients[expressionIn, unknownCoefList,        *)
(*     parameters]                                                    *)
(* Purpose: To extract a linear system to solve for the coefficients  *)
(* Input:   An expression in euler form containing coefficients c[i]  *)
(*          List of unknown coefficients                              *)
(*          List of all parameters in the problem                     *)
(* Output:  A list of coefficient expressions                         *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whexsyco.m, Created 7 June 2004 at CSM              *)
(* Code is in File:  independence/dpexsyc3.m                          *)
(* Major Changes Made: 13 September, 2007, by DP at CSM.   Revisions  *)
(*      have completely changed this function from its original form. *)
(* Last Modified:  12 November, 2008, 18:51 by DP at CSM              *)
(**********************************************************************)

extractSystemForCoefficients[expressionIn_, unknownCoefList_, parameters_] :=
Module[{expandedExpressionIn, termsToRemove, coefficientExpns, loneTerms,
    conHead = Head[Part[unknownCoefList, 1]], recipTermsToRemove, times, i1,
    extraTermRule, functionWithExplicitRule, explicitIndVarRule, expnList,
    termsWithU, termsWithDerU, termsWithExpl, printESFC},

  If[debugExtractSystemForCoefficients, printESFC = Print, Clear[printESFC],
      Clear[printESFC]];
  printESFC["debug code: ESFC, Function: extractSystemForCoefficients,"<>
      " File: dpexsyc3.m"];

  If[expressionIn === 0, Return[{}]];

  (* Initialization *)
  expandedExpressionIn = Expand[expressionIn];

  printESFC["At ESFC IN, the coefficient expression is:"];
  printESFC[expandedExpressionIn];
  printESFC["At ESFC IN, unknown coefficients to find:"];
  printESFC[unknownCoefList];
  printESFC["At ESFC IN, a list of parameters given: ", parameters];
  printESFC["At ESFC IN, the head on the unknown constants is ", conHead];

  expandedExpressionIn = If[Head[expandedExpressionIn] === conHead,
      {expandedExpressionIn}, expandedExpressionIn];

  (* Find the positions of the terms that have c[i] sitting alone.      *)
  expnList = Apply[List, expandedExpressionIn];
  termsWithU = Position[Map[MemberQ[#, u[_][__], {0, Infinity}] &,
      expnList], True];
  printESFC["At ESFC 1, the positions of all terms containing u[_][__]:"];
  printESFC[termsWithU];

  termsWithDerU = Position[Map[MemberQ[#, Derivative[__][u[_]][__],
      {0, Infinity}] &, expnList], True];
  printESFC["At ESFC 2, the positions of all terms containing"<>
      " Derivative[__][u[_]][__]:"];
  printESFC[termsWithDerU];

  loneTerms = Delete[expandedExpressionIn, Union[termsWithU, termsWithDerU]];
  printESFC["At ESFC 3, all terms with u[_][__] and"<>
      " Derivative[__][u[_]][__] have been removed from the expression:"];
  printESFC[loneTerms];

  termsWithExpl = Position[Map[MemberQ[#, n1_ /; MemberQ[{x, y, z, t}, n1],
      {0, Infinity}] &, Apply[List, loneTerms]], True];
  printESFC["At ESFC 4, the positions of all terms containing stand alone"<>
      " explicit independent variables:"];
  printESFC[termsWithExpl];

  loneTerms = Delete[loneTerms, termsWithExpl];
  printESFC["At ESFC 5, all ", conHead, "[i] not connected to"<>
      " u[_][__], its derivatives, or any explicit independent variables:"];
  printESFC[loneTerms];

  workingVarDerList = expandedExpressionIn - loneTerms;
  printESFC["At ESFC 6, the coefficient expression list after lone"<>
      " constants are removed:"];
  printESFC[workingVarDerList];

  If[workingVarDerList =!= {0} && workingVarDerList =!= 0,
    (* Make a list of terms needed to identify common coefficients.     *)
    termsToRemove = Union[stripper[workingVarDerList, {}]];
    printESFC["At ESFC 7, stripper is used to identify terms used as a"<>
        " basis for forming coefficient equations:"];
    printESFC[termsToRemove];

    (* Match all terms in the given expression according to the terms   *)
    (* given in the previous list.                                      *)
    coefficientExpns = Table[Cases[workingVarDerList,
        Times[___, Part[termsToRemove, i1]], {0, 1}],
        {i1, 1, Length[termsToRemove]}];
    printESFC["At ESFC 8, the expression(s) from the common terms list"<>
        " after they have been matched to the stripped terms. Note: not"<>
        " all u[_] may have been removed at this point."];
    printESFC[coefficientExpns];

    (* Remove all parts that are not coefficients.                       *)
    recipTermsToRemove = Map[Power[#, -1] &, termsToRemove];
    printESFC["At ESFC 9, the reciprocal applied to the terms to be removed"<>
        " list:"];
    printESFC[recipTermsToRemove];

    coefficientExpns = Thread[times[coefficientExpns, recipTermsToRemove]] /.
        times -> Times;
    printESFC["At ESFC 10, terms with extra u[_] have been exposed:"];
    printESFC[coefficientExpns];

    extraTermRule = {Derivative[__][u[_]][__] -> 0, u[_][__] -> 0};
    functionWithExplicitRule = _[n1__] /;
        MemberQ[{n1}, Alternatives[x, y, z, t]] -> 0;
    explicitIndVarRule = {x -> 0, y -> 0, z -> 0, t -> 0};

    coefficientExpns = coefficientExpns /. extraTermRule;
    coefficientExpns = coefficientExpns /. functionWithExplicitRule;
    coefficientExpns = coefficientExpns /. explicitIndVarRule;
    printESFC["At ESFC 11, all extra u[_] have been removed leaving a"<>
        " list of coefficient expressions:"];
    printESFC[coefficientExpns];

    coefficientExpns = Apply[Plus, coefficientExpns, {1}];
    printESFC["At ESFC 12, Plus has been applied to each group of terms:"];
    printESFC[coefficientExpns],

  (* else *)
    coefficientExpns = {}
  ]; (* end If workingVarDerList *)

  (* Put in expressions formed using lone constants.                    *)
  coefficientExpns = Union[coefficientExpns, {loneTerms}];
  coefficientExpns = Flatten[DeleteCases[coefficientExpns, 0, {1}]];
  printESFC["At ESFC 13, lone constant expressions have been added to the"<>
      " list of coefficient equations:"];
  printESFC[coefficientExpns];

  Clear[expandedExpressionIn, termsToRemove, loneTerms, conHead, times,
      recipTermsToRemove, i1, extraTermRule, functionWithExplicitRule,
      explicitIndVarRule, printESFC];

  Return[coefficientExpns]
] (* end Module extractSystemForCoefficients *)

(* ##########           Mini-function: factorSign          ########## *)

(**********************************************************************)
(* factorSign[equationList, parameters, conHead]                      *)
(* Purpose: To remove equations from a list of equations where if     *)
(*          the equation were multiplied through by -1, the result    *)
(*          would duplicate another equation in the equation list.    *)
(* Input:   A list coefficient equations                              *)
(*          A list of all parameters                                  *)
(*          The head of the undetermined coefficients                 *)
(* Output:  A shorter list of equations                               *)
(* Created: 22 May, 2008 by DP at CSM                                 *)
(* Code is in File:  conservationlaws/nwgensol.m                      *)
(* Last Modified:  16 February, 2009, 16:55 by DP at CSM              *)
(**********************************************************************)

factorSign[listOfEqns_List, parameters_, conHead_] :=
Module[{signsOnTerms, positiveSigns, negativeSigns, signLeadingTerm,
    changeSign, poszero, paramRule, newEqnList, n1, gr, sa, equationList},

  If[debugFactorSign, printFS = Print, Clear[printFS], Clear[printFS]];

  printFS["debug code: FS, Function: factorSign, File: nwgensol.m"];

  printFS["At FS IN, the list of equations:"];
  printFS[listOfEqns];
  printFS["At FS IN, the list of parameters contains: ", parameters];

  equationList = Map[Expand, listOfEqns, {1,3}];
  printFS["At FS 0, the equations in expanded form:"];
  printFS[equationList];

  paramRule = Map[Rule[#, 1] &, parameters];

  signsOnTerms =  Map[Sign, Apply[List, Map[Part[#, 1] &, equationList],
      {1}], {2}] /. paramRule /. Sign[conHead[_]] -> 1;
  printFS["At FS 1, a list of 1 and -1 showing signs on the terms of the"<>
      " left-hand side of each equation:"];
  printFS[signsOnTerms];

  positiveSigns = Map[Count[#, 1] &, signsOnTerms];
  printFS["At FS 2, the number of positive signs in each equation:"];
  printFS[positiveSigns];

  negativeSigns = Map[Count[#, -1] &, signsOnTerms];
  printFS["At FS 3, the number of negative signs in each equation:"];
  printFS[negativeSigns];

  comparePosNeg = Thread[List[Thread[gr[positiveSigns, negativeSigns]],
      Thread[sa[positiveSigns, negativeSigns]]]] /. {gr -> Greater,
      sa -> SameQ} /. {{False, False} -> -1, {False, True} -> 0,
      {True, False} -> 1};
  printFS["At FS 4, checking to see if the number of positive signs is"<>
      " greater than the number of negative signs,"<>
      " -1 means more negative signs, 0 means the number is equal, and"<>
      " 1 means more positive signs:"];
  printFS[comparePosNeg];

  signLeadingTerm = Map[Sign, Map[Part[#, 1] &, signsOnTerms]];
  printFS["At FS 5, 1 and -1 indicate the signs on the leading terms:"];
  printFS[signLeadingTerm];

  changeSign = Thread[List[comparePosNeg, signLeadingTerm]];
  printFS["At FS 6, the comparison lists for positive and negative signs"<>
      " and signs on leading terms have been combined:"];
  printFS[changeSign];

  poszero = Position[comparePosNeg, 0];
  changeSign = MapAt[pl, changeSign, poszero] /. pl[{n1__}] -> pl[n1] /.
      pl -> Plus;
  printFS["At FS 7, the positions in the list where positive and negative"<>
      " signs are balanced are analyzed:  if the leading term is positive,"<>
      " 1 is assigned and if the leading term is negative, -1 is assigned."];
  printFS[changeSign];

  changeSign = changeSign /. {n1_, _} -> n1;
  printFS["At FS 8, the rest of the equations are analyzed according to the"<>
      " number of positive signs: -1 means all signs in the equation will,"<>
      " be changed and 1 means all signs will stay the same:"];
  printFS[changeSign];

  newEqnList = Expand[Thread[Times[changeSign, Map[Part[#, 1] &,
      equationList]]]];
  newEqnList = Map[Factor, newEqnList];
  printFS["At FS 9, the signs are changed on the left-hand sides:"];
  printFS[newEqnList];

  newEqnList = Union[Map[Equal[#, 0] &, newEqnList]];
  printFS["At FS 10, equations rebuilt with the correct signs:"];
  printFS[newEqnList];

  Clear[signsOnTerms, positiveSigns, negativeSigns, signLeadingTerm,
      changeSign, poszero];

  Return[newEqnList]
] (* end Module factorSign *)

(* ##########            Function: generalSolver           ########## *)

(**********************************************************************)
(* generalSolver[systemListWithUnknowns, rhoWithConstants, parameters,*)
(*     weightedParameters, numDepVars, indepVarsList, rankCan]        *)
(* Purpose: To solve a system of linear equations formed from         *)
(*          applying the Euler operator to the t-derivative of a      *)
(*          candidate density containing undetermined coefficients.   *)
(*          If needed, the solver will also determine compatability   *)
(*          conditions for parameters.                                *)
(* Input:   A list of all c[i], with a system of coefficient          *)
(*              equations developed by setting the variational        *)
(*              derivative on the t-derivative of the density equal   *)
(*              to zero.                                              *)
(*          The candidate density with undetermined constants, c[i].  *)
(*          A list of nonweighted parameters in the PDE.              *)
(*          A list of weighted parameters in the PDE.                 *)
(*          The number of dependent variables                         *)
(*          A list of independent variables                           *)
(*          The rank of the candidate density                         *)
(* Output:  Final form(s) for rho along with any determined           *)
(*          compatibility conditions determined by the parameters.    *)
(* Adapted From:  data/newzeala/condens/c060717h.m                    *)
(*                Created 3 July 2006 by WH at home                   *)
(* Code is in File:  conservationlaws/nwgensol.m                      *)
(* Last Modified:  15 October, 2009, 15:12 by DP at CSM               *)
(**********************************************************************)

generalSolver[systemListWithUnknowns_List, rhoWithConstants_, parameters_,
    weightedParameters_, numDepVars_, indepVarsList_, rankCan_] :=
Module[{rhoResultWithCompat, analyzeListStart, analyzeListEnd, mainEqnList,
    zerocRulesList, nonzerocList, checkSysForParameters, systemcond,
    lthsys, inputList, inputpart, inputval, inputrule, cReqSolnList,
    comcond, myruletrick, comcondfactab, comcondfac, k, lhs, rhs,
    flip,  sol1, sol2, n1, n2, rootfindersol1, rootfindersol2, finalsol,
    lengthsol1, iii,  par, nrules1 = {}, lengthpar = Length[parameters],
    newmainEqnList, newestRho, solc,  rhoResult = {}, nRules1List = {},
    nsolc, printGS, passTime = AbsoluteTime[]},

  If[debugGeneralSolver, printGS = Print, Clear[printGS], Clear[printGS]];

  printGS["debug code: GS, Function: generalSolver, File: nwgensol.m"];

  printGS["At GS IN, the system of undetermined constants given:"];
  printGS[Part[systemListWithUnknowns, 1]];
  printGS["At GS IN, a list of undetermined coefficients"];
  printGS[Part[systemListWithUnknowns, 2]];
  printGS["At GS IN, rules for coefficients known to be zero:"];
  printGS[Part[systemListWithUnknowns, 3]];
  printGS["At GS IN, the current candidate density is:"];
  printGS[rhoWithConstants];
  printGS["At GS IN, a list of unweighted parameters is:"];
  printGS[parameters];
  printGS["At GS IN, a list of weighted parameters is:"];
  printGS[weightedParameters];
  printGS["At GS IN, there are ", numDepVars, " dependent variables."];
  printGS["At GS IN, the independent variables are: ", indepVarsList];
  printGS["At GS IN, the rank of the candidate is ", rankCan];

  lthsys = Length[Part[systemListWithUnknowns, 1]];
  printLOW["------------------------------------------"];
  printLOW["*** Analyzing Candidate Density No. ", globalKcounter, " with"<>
      " Rank ", rankCan, " ***"].
  printLOW["The system of undetermined coefficients, c[i], for candidate"<>
      " density ", globalKcounter, " with rank ", rankCan, " has"<>
      " ", lthsys, " equation"<>If[lthsys =!= 1, "s.", "."]];
  printHIGH[Map[Equal[#, 0] &,
      MapAll[Factor, Part[systemListWithUnknowns, 1]]]];
  globalKcounter++;

  (* If the system of coefficients is an empty list, there are no       *)
  (* parameter compatibility conditions to calculate and there is no    *)
  (* system to solve.  The density term remains unchanged.              *)
  If[Part[systemListWithUnknowns, 1] === {} && zerocRulesList === {},
    printLOW["There is no system to solve."];
    rhoResultWithCompat = evaluateRho[{rhoWithConstants}, parameters,
    weightedParameters, {}, numDepVars, indepVarsList, rankCan];

    printGS["At GS OUT, the density has been determined as:"];
    printGS[Part[rhoResultWithCompat, 1]];

    Return[rhoResultWithCompat]
  ]; (* end If Part[systemListWithUnknowns, 1] *)

  (* Start of simplification and analysis of the system.                *)
  (* First remove all c[i] that are obviously zero.                     *)
  analyzeListStart = {Part[systemListWithUnknowns, 1] /.
      Part[systemListWithUnknowns, 3], {}};
  printGS["At GS -6, entering analyzeForZeroConstants, with"<>
      " analyzeListStart:"];
  printGS[analyzeListStart];

  analyzeListEnd = FixedPoint[analyzeForZeroConstants, analyzeListStart, 250];
  printGS["At GS -5, leaving analyzeForZeroConstants, with"<>
      " analyzeListEnd:"];
  printGS[analyzeListEnd];

  mainEqnList = Map[Equal[#, 0] &, Part[analyzeListEnd, 1]];
  printGS["At GS -4, leaving analyzeForZeroConstants, with"<>
      " mainEqnList:"];
  printGS[mainEqnList];

  mainEqnList = factorSign[mainEqnList, Union[parameters,
      weightedParameters], c];
  printGS["At GS -3, leaving factorSign, with all duplicate equations"<>
      " removed from the main equation list:"];
  printGS[mainEqnList];

  zerocRulesList = Union[Map[Rule[#, 0] &, Part[analyzeListEnd, 2]],
      Part[systemListWithUnknowns, 3]];
  printGS["At GS -2, leaving analyzeForZeroConstants, with"<>
      " zerocRulesList:"];
  printGS[zerocRulesList];

  nonzerocList = Complement[Part[systemListWithUnknowns, 2],
      Union[Part[analyzeListEnd, 2], Part[systemListWithUnknowns, 3] /.
      Rule[n1_, 0] -> n1]];
  printGS["At GS -1, leaving newanalyzer, with nonzerocList:"];
  printGS[nonzerocList];

  printHIGH["Initial analysis of the system of undetermined coefficients"<>
      " shows that these c[i] must be zero:"];
  printHIGH[zerocRulesList];

  printHIGH["The system of c[i] is simplified by replacing all c[i] -> 0" <>
      " with 0, factoring and removing common factors, and"<>
      " by removing duplicate equations. The updated system has "<>
      ToString[Length[mainEqnList]]<>" equations:"];
  printHIGH[mainEqnList];

  (* No need to test for compatibility conditions if the system has     *)
  (* been completely eliminated.                                        *)
  If[mainEqnList === {} && nonzerocList === {},
    printLOW["All c[i] in the system have been found to be zero."<>
        "  No density exists for this candidate."];
    rhoResultWithCompat = evaluateRho[{rhoWithConstants} /. zerocRulesList,
        parameters, weightedParameters, {}, numDepVars, indepVarsList, rankCan];

    printGS["At GS OUT, the density has been determined as:"];
    printGS[Part[rhoResultWithCompat, 1]];

    (* Clear all local variables not being returned.                      *)
    Clear[analyzeListStart, analyzeListEnd, mainEqnList, zerocRulesList,
        nonzerocList, printGS, printGS];

    Return[rhoResultWithCompat]
  ]; (* end If mainEqnList *)

  (* Check to see if parameters were removed from the main equation     *)
  (* list when zero c[i] were replaced by zero.                         *)
  If[parameters =!= {},
    checkSysForParameters =
        Map[MemberQ[mainEqnList, #, {0, Infinity}] &, parameters];
    If[Union[checkSysForParameters] === {False},
      printLOW["All parameters were eliminated from the system when"<>
          " constants in equations containing parameters were found to"<>
          " be zero.  It is assumed the conservation law(s) will hold"<>
          " for all nonzero parameter values."];
    ], (* end If !Union *)
  (* else *)
    checkSysForParameters = {False}
  ]; (* end If parameters *)

  (* Case I:  Parameters are present - check for compatibility           *)
  (* conditions.                                                         *)
  If[Union[checkSysForParameters] =!= {False},
                                          (* then 8 start parameter test *)
    systemcond = Map[!Equal[#, 0] &, parameters];
    systemcond = Apply[And, systemcond];
    printLOW["Searching for compatibility conditions."];
    printLOW["The software will assume that ",systemcond,"."];
    printLOW["Cases where one or more of these parameters are zero must be"<>
        " run separately!"];

    printGS["At GS 1, each c[i] in the nonzerocList will be set to one"<>
        " (one at the time). The nonzerocList contains:"];
    printGS[nonzerocList];

    (* Check for an empty list of nonzero coefficients.                  *)
    If[nonzerocList === {},
      Print["All of the coefficients c[i] in the density have to vanish."];
      Print["The search for compatibility conditions ends."],
    (* else *)
      printHIGH["The program will set the nonzero coefficients equal to 1,"<>
          " one at the time."]
    ]; (* end If nonzerocList *)

    inputList = nonzerocList;
    While[inputList =!= {},

      (* Set first element in list of unknowns equal to 1.               *)
      inputpart = Part[inputList, 1];
      inputval = inputpart == 1;
      cReqSolnList = Complement[nonzerocList, {inputpart}];
      (* Double check that c[1] is not zero.                             *)
      inputrule = ToRules[inputval];
      printGS["At GS 2, inputpart: ", inputpart, "\ninputval: ", inputval,
          "\ninputrule: ", inputrule];

      printHIGH["------------------------------------------"];
      printHIGH["* Setting ", inputrule[[1]],":"];
      printHIGH["Computation of the compatibility conditions."];

      If[globalVerbose =!= High,
        passTime = AbsoluteTime[] - passTime;
        If[passTime > 10, Print["Computing compatibility conditions..."]];
        passTime = AbsoluteTime[]
      ]; (* end If globalVerbose *)

      printGS["At GS 3a, entering Eliminate after applying inputrule,"<>
          " mainEqnList:"];
      printGS[mainEqnList /. inputrule];
      printGS["At GS 3b, entering Eliminate, cReqSolnList:"];
      printGS[cReqSolnList];

      (* Simplify system down to parameters                              *)
      comcond = Eliminate[mainEqnList /. inputrule, cReqSolnList];
      printGS["At GS 4, leaving Eliminate, comcond:"];
      printGS[comcond];

      (* Move terms to left hand side of equation.                       *)
      myruletrick = {lhs__ == rhs__ -> lhs - rhs == 0};
      comcond = comcond /. myruletrick;
      printGS["At GS 5, moving all terms to left hand side, comcond:"];
      printGS[comcond];

      (* Factor the left hand side.                                      *)
      comcondfac = MapAll[Factor, comcond];
      printGS["At GS 6, after MapAll factor on comcond, comcondfac:"];
      printGS[comcondfac];

      (* Remove possible duplicates *)
      If[Head[comcondfac] === Or,
        comcondfactab = Table[Part[comcondfac, k], {k, 1, Length[comcondfac]}];
        comcondfac = Union[comcondfactab];
        printGS["At GS 7, after removing possible duplicates, comcondfac:"];
        printGS[comcondfac]
      ]; (* end If head *)

      printHIGH["This is the compatibility condition:"];
      printHIGH[comcondfac];

      (* LogicalExpand added to make it work under Math v. 5.0              *)
      (* Reduce system to isolate compatibility conditions.                 *)
      printGS["At GS 8, entering Reduce, parameters:"];
      printGS[parameters];
      printGS["At GS 9, entering Reduce, comcond && systemcond:"];
      printGS[comcond && systemcond];

      (* TO DO 01/23/07 Temporary fix for Reduce - removes square roots *)
      (* in the solutions for the fifth order KdV case, but may not     *)
      (* work for other cases.                                          *)
      flip[equ_] := equ /. {Equal[n1_, n2_] -> Equal[n2, n1]};
      sol1 = LogicalExpand[Reduce[comcond && systemcond, parameters]];
      sol2 = LogicalExpand[Reduce[comcond && systemcond, Reverse[parameters]]];
      sol2 = Map[flip, sol2];
      rootfindersol2 = Cases[sol2, Power[_,1/2], Infinity];
      If[rootfindersol2 === {}, finalsol = sol2];
      rootfindersol1 = Cases[sol1, Power[_,1/2], Infinity];
      If[rootfindersol1 === {}, finalsol = sol1];
      sol1 = finalsol;
      printGS["At GS 9a, the results from Reduce:"];
      printGS[sol1];

      If[comcondfac === True,
        printHIGH["The compatibility condition is satisfied without constraints"<>
            " on the parameters."];
      ]; (* end If comcondfac *)

      If[sol1 =!= False, (* then 4 *)

      (* Cases where sol1 has Or in it.                              *)
        If[!FreeQ[sol1, Or], (* then 5 *)
          lengthsol1 = Length[sol1];
          For[iii = 1, iii <= lengthsol1, iii++,
            printGS["At GS 10, taking sol1[[", iii, "]]:"];
            printGS[Part[sol1, iii]];

            nrules1 = ToRules[Part[sol1, iii]];
            printGS["At GS 11, nrules1 : "];
            printGS[nrules1];

            If[comcondfac =!= True,
              par = parameters /. nrules1;
              For[k = 1, k <= lengthpar, k++,
                printGS["At GS 12, before applying nrules1, "<>
                    " parameters[[",  k, "]] = ", parameters[[k]]];

                printGS["At GS 13, after applying nrules1, "<>
                    " par[[", k, "]] = ", par[[k]]];

                printHIGH["For ", parameters[[k]], " = ", par[[k]]];
              ] (* end For k = 1 *)
            ]; (* end If comcondfac *)

            printGS["At GS 14, before applying nrules1, mainEqnList:"];
            printGS[mainEqnList];

            newmainEqnList = mainEqnList //. nrules1;
            printGS["At GS 15, after applying nrules1, newmainEqnList:"];
            printGS[newmainEqnList];

            newmainEqnList = newmainEqnList /. inputrule;
            printGS["At GS 16, after applying inputrule, going into"<>
                " solver newmainEqnList: "];
            printGS[newmainEqnList];

            printGS["At GS 17, unknowns going into solver, cReqSolnList:"];
            printGS[cReqSolnList];

            solc = Flatten[Solve[newmainEqnList, cReqSolnList]];
            printGS["At GS 18, after solving for c[i], solc: "];
            printGS[solc];

            If[solc === {} || Union[Map[Part[#, 2] &, solc]] === {0} &&
                newmainEqnList =!= {True},
              printHIGH["The system becomes inconsistent, or all of the c[i]"<>
                  " must vanish.  No density exists for the case where"<>
                  " ", Part[inputrule, 1], "."],

            (* else *)
              solc = Union[solc, inputrule];
              printGS["At GS 19, after union with inputrule, solc: "];
              printGS[solc];

              solc = Union[solc, zerocRulesList];
              printGS["At GS 20, after union with zerocRulesList, solc: "];
              printGS[solc];

              printHIGH["The solution of the system is:"];
              printHIGH[solc];
              printHIGH[If[Union[solc /. Rule[c[_], 0] -> 0] === {0},
                  "All c[i] in the system have been found to be"<>
                  " zero.  No density exists for this candidate.",
                  "The density formed from this candidate will be"<>
                  " analyzed and compared to other densities.  Its final"<>
                  " form will be reported in the results at the"<>
                  " conclusion of the program."]];

              printGS["At GS 21, nrules1: "];
              printGS[nrules1];

              (* Append newest rho to a list of previously calculated    *)
              (* rhos.  Keep compatibility conditions in a separate list *)
              (* corresponding to the rhos in the list of rhos.          *)
              newestRho = Expand[rhoWithConstants /. solc /. nrules1];
              If[newestRho =!= 0,
                rhoResult = Append[rhoResult, newestRho];
                printGS["At GS 22, rhos determined so far:"];
                printGS[rhoResult];
                nRules1List = Append[nRules1List, nrules1];
                printGS["At GS 23, corresponding compatibility conditions:"];
                printGS[nRules1List];
              ] (* end If newestRho *)
            ]; (* end If solc *)

            (* Case with parameters and sol1 has Or in it.              *)
            (* Begin new routine for compatibility analysis.            *)
            If[inputList =!= {},
              inputList = Complement[inputList, {inputpart}];
              printGS["At GS 24, case with parameters, after removing"<>
                  " the first element, updated inputList:"];
              printGS[inputList],
            (* else *)
              inputList = {}
            ]; (* end If inputList *)

            (* Case with parameters and sol1 has Or in it.              *)
            (* End new routine for compatibility analyis.               *)

            Clear[solc, newmainEqnList];
            nrules1 = {}
          ], (* end For iii = 1 *)

        (* else 5 where sol1 has no Or in it *)
          nrules1 = ToRules[sol1];
          printGS["At GS 25, nrules1:"];
          printGS[nrules1];

          If[comcondfac =!= True, (* then 6 *)
            par = parameters /. nrules1;
            For[k = 1, k <= lengthpar, k++,
              printGS["At GS 26, before applying nrules1,
                  parameters[[", k, "]] = ", parameters[[k]]];

              printGS["At GS 27, after applying nrules1,
                  par[[", k, "]] = ", par[[k]]];

              printHIGH["For ", parameters[[k]], " = ", par[[k]]];
            ] (* end For k = 1 *)
          ]; (* end If comcondfac 6 *)

          printGS["At GS 28, before applying nrules1, mainEqnList:"];
          printGS[mainEqnList];

          newmainEqnList = mainEqnList //. nrules1;
          printGS["At GS 29, after applying nrules1, newmainEqnList:"];
          printGS[newmainEqnList];

          newmainEqnList = newmainEqnList /. inputrule;
          printGS["At GS 30, after applying inputrule, going into solver"<>
              " newmainEqnList: "];
          printGS[newmainEqnList];

          printGS["At GS 31, unknowns going into solver, cReqSolnList:"];
          printGS[cReqSolnList];

          solc = Flatten[Solve[newmainEqnList, cReqSolnList]];
          printGS["At GS 32, after solving for c[i], solc:"];
          printGS[solc];

          If[solc === {} && Length[nonzerocList] > 1,
            printHIGH["The system becomes inconsistent, or all of the c[i]"<>
                " must vanish.  No density exists for the case where"<>
                " ", Part[inputrule, 1], "."],

          (* else *)
            solc = Union[solc, inputrule];
            printGS["At GS 33, after union with inputrule, solc:"];
            printGS[solc];

            solc = Union[solc,zerocRulesList];
            printGS["At GS 34, after union with zerocRulesList, solc:"];
            printGS[solc];

            printHIGH["The solution of the system is "];
            printHIGH[solc];
            printHIGH[If[Union[solc /. Rule[c[_], 0] -> 0] === {0},
                "All c[i] in the system have been found to be"<>
                " zero.  No density exists for this candidate.",
                "The density formed from this candidate will be"<>
                " analyzed and compared to other densities.  Its final"<>
                " form will be reported in the results at the"<>
                " conclusion of the program."]];

            printGS["At GS 35, nrules1: "];
            printGS[nrules1];

            (* Append newest rho to a list of previously calculated rhos.  *)
            (* Keep compatibility conditions in a separate list            *)
            (* corresponding to the rhos in the list of rhos.              *)
            newestRho = Expand[rhoWithConstants /. solc /. nrules1];
            If[newestRho =!= 0,
              rhoResult = Append[rhoResult, newestRho];
              printGS["At GS 36, rhos determined so far:"];
              printGS[rhoResult];
              nRules1List = Append[nRules1List, nrules1];
              printGS["At GS 37, corresponding compatibility conditions:"];
              printGS[nRules1List];
            ]; (* end if newestRho *)
          ]; (* end If solc === {} *)

          (* Case with parameters and sol1 has no Or in it.              *)
          (* Begin new routine for compatibility analysis.               *)

          If[inputList =!= {}, inputList = Complement[inputList, {inputpart}];
            printGS["At GS 38, case with parameters, after"<>
                " removing the first element, updated inputList: "];
            printGS[inputList],

          (* else *)
            inputList = {}
          ]; (* end If inputList *)

          (* Case with parameters and sol1 had no or in it.              *)
          (* End new routine for compatibility analysis.                 *)

          Clear[solc, newmainEqnList];
          nrules1 = {}
        ], (* end If 5 case where sol1 had Or in it *)

      (* else 4 sol1 was false *)
        printHIGH["The system becomes inconsistent, or the compatibility"<>
            " conditions require that one or more of the parameters"<>
            " are zero.  No compatibility conditions exist for the"<>
            " case where ", Part[inputrule, 1], "."];

        (* Begin new routine for compatibility analysis.               *)
        If[inputList =!= {}, inputList = Complement[inputList, {inputpart}];
          printGS["At GS 39, case with parameters, after"<>
              " removing the first element, updated inputList: "];
          printGS[inputList],

        (* else *)
          inputList = {}
        ] (* end If inputList *)
      ] (* end If 4 *)
    ], (* closes While *)

  (* else 8 Case2: case without parameters *)
  (* mainEqnList has already been simplified by removing all zero c[i]. *)

    nsolc = Flatten[Solve[mainEqnList, nonzerocList]];
    printGS["At GS 40, after solving (case without parameters), nsolc:"];
    printGS[nsolc];

    nsolc = Union[zerocRulesList, nsolc];

    printHIGH["The solution of the system is:"];
    printHIGH[nsolc];
    printHIGH[If[Union[nsolc /. Rule[c[_], 0] -> 0] === {0},
        "All c[i] in the system have been found to be"<>
        " zero.  No density exists for this candidate.",
        "The density formed from this candidate will be"<>
        " analyzed and compared to other densities.  Its final"<>
        " form will be reported in the results at the"<>
        " conclusion of the program."]];

    rhoResult = {rhoWithConstants /. nsolc};
    nRules1List={};
    Clear[nsolc]
  ]; (* end 8 end if parameter parameter test *)

  printGS["At GS 41, the list of rhos in rhoResult:"];
  printGS[rhoResult];

  nRules1List = nRules1List/. {{}}->{};

  If[rhoResult =!= {},
    rhoResultWithCompat = evaluateRho[rhoResult, parameters,
        weightedParameters, nRules1List, numDepVars, indepVarsList, rankCan],

  (* else *)
    rhoResultWithCompat = {{}, {}}
  ]; (* end If rhoResult *)

  printGS["At GS OUT, a two-part list contains\n"<>
      " {known density, compatibility conditions}:"];
  printGS[rhoResultWithCompat];

  (* Clear all local variables not being returned.                      *)
  Clear[analyzeListStart, analyzeListEnd, mainEqnList, zerocRulesList,
      nonzerocList, checkSysForParameters, systemcond, inputList, inputpart,
      inputval, inputrule, cReqSolnList, comcond, myruletrick, comcondfac,
      comcondfactab, comcondfac, k, lhs, rhs, flip, sol1, sol2, n1, n2,
      rootfindersol1, rootfindersol2, finalsol, lengthsol1, iii, nrules1,
      par, lengthpar, newmainEqnList, solc, newestRho, rhoResult, nRules1List,
      nsolc, printGS, printGS];

  Return[rhoResultWithCompat]
]; (* end Module generalSolver *)

(* ##########      Function: analyzeForZeroConstants       ########## *)

(**********************************************************************)
(* analyzeForZeroConstants[doublelist]                                *)
(* Purpose: To determine the coefficients c[i] that must be zero,     *)
(*          dynamically simplifies system and makes list of c[i]      *)
(*          that must be zero used in the search for compatibility    *)
(*          conditions.                                               *)
(* Input:   A double list containing the system being analyzed and    *)
(*          a list of c[i] known to be zero.                          *)
(* Output:  A double list containing the simplified system and a      *)
(*          list of c[i] known to be zero.                            *)
(* Adapted From:  data/newzeala/condens/c060717h.m,                   *)
(*                Created 13 July 2006 by WH at home                  *)
(* Code is in File:  conservationlaws/nwanazco.m                      *)
(* Last Modified:  29 April, 2008, 18:04 by DP at CSM                 *)
(**********************************************************************)

(* analyzelist will be list of two pieces,                            *)
(* first piece:  list of simplified lhs of equations                  *)
(* second piece: list of the c[i] that must be zero                   *)

analyzeForZeroConstants[doublelist_List] :=
Module[{currentsystem, currentzeroc, strippedsystem, updatedzeroc,
    analyzelist, updatedzerocrules, updatedsystem, printAFZC},

  If[debugAnalyzeForZeroConstants, printAFZC = Print, Clear[printAFZC],
      Clear[printAFZC]];

  printAFZC["debug code: AFZC, Function: analyzeForZeroConstants,"<>
      " File: nwanazco.m"];

  printAFZC["Starting repeated analysis and simplification of the system."];
  currentsystem = Part[doublelist, 1];
  printAFZC["At AFZC IN in newanalyzer, currentsystem: "];
  printAFZC[currentsystem];

  currentzeroc = Part[doublelist, 2];
  printAFZC["At AFZC IN in newanalyzer, currentzeroc: "];
  printAFZC[currentzeroc];

  strippedsystem = Union[Map[zeroFactorStripper, currentsystem]];
  printAFZC["At AFZC 1 in newanalyzer, after mapping zeroFactorStripper,"<>
      " strippedsystem: "];
  printAFZC[strippedsystem];

  updatedzeroc = Union[Join[currentzeroc, Cases[strippedsystem, _c, 1]]];
  printAFZC["At AFZC 2 in newanalyzer, updatedzeroc: "];
  printAFZC[updatedzeroc];

  updatedzerocrules = Union[Map[Rule[#, 0] &, updatedzeroc]];
  printAFZC["At AFZC 3 in newanalyzer, updatedzerocrules: "];
  printAFZC[updatedzerocrules];

  updatedsystem = Complement[strippedsystem /. updatedzerocrules, {0}];
  printAFZC["At AFZC 4 in newanalyzer, after application of"<>
      " updatedzerocrules, updatedsystem: "];
  printAFZC[updatedsystem];

  analyzelist = {updatedsystem, updatedzeroc};
  printAFZC["At AFZC 5 in newanalyzer, analyzelist: "];
  printAFZC[analyzelist];

  printAFZC["  !!!!!!!!! END OF ONE RUN OF NEWANALYZER !!!!!!!!!!  "];

  (* Clear all local variables not being returned.                      *)
  Clear[currentsystem, currentzeroc, strippedsystem, updatedzeroc,
      updatedzerocrules, updatedsystem, printAFZC];

  Return[analyzelist]
]; (* end Module analyzeForZeroConstants *)

(* ##########        Function: zeroFactorStripper          ########## *)

(**********************************************************************)
(* zeroFactorStripper[givenExpression]                                *)
(* Purpose: To cancel the numerical factors and members of nzsymbols, *)
(*          and anything that has no C in each term of the argument.  *)
(* Input:   Expression with a coefficient containing c[i] and         *)
(*          parameters.                                               *)
(* Output:  Expression stripped of any factors that do not contain    *)
(*          a c[i] and will not require a zero parameter.             *)
(* Adapted From:  data/newzeala/condens/c060717h                      *)
(*                Created 11 August, 2006 by WH at home               *)
(* Code is in File:  conservationlaws/nwzfastr.m                      *)
(* Last Modified:  30 April, 2008, 10:27 by DP at CSM                 *)
(**********************************************************************)

zeroFactorStripper[givenExpression_] := Module[
    {faclist, factorsfreeofc, factorswithc, factorstokeep, factorstocancel,
    factorscancelled, freeofc, notfreeofc, givenexpr, strippedequation},

  If[debugZeroFactorStripper, printZFS = Print, Clear[printZFS],
      Clear[printZFS]];

  printZFS["debug code: ZFS, Function: zeroFactorStripper, File: nwzfastr.m"];

  freeofc[x_] := FreeQ[x, c];
  notfreeofc[x_] := Not[FreeQ[x, c]];

  givenexpr = Factor[givenExpression];
  printZFS["At ZFS 1, factored form givenexpr: ", givenexpr];

  If[Head[givenexpr] === Times,
    faclist = FactorList[givenexpr];
    printZFS["At ZFS 2, faclist: ", faclist];

    factorsfreeofc = Select[faclist, freeofc[#] &];
    printZFS["At ZFS 3, factorsfreeofc: ", factorsfreeofc];

    factorswithc = Complement[faclist, factorsfreeofc];
    printZFS["At ZFS 4, factorswithc: ", factorswithc];

    factorstocancel = Select[factorsfreeofc, (Head[Part[#, 1]] =!= Plus) &];
    printZFS["At ZFS 5, factorstocancel: ", factorstocancel];

    factorstokeep = Select[factorsfreeofc, (Head[Part[#, 1]] === Plus) &];
    printZFS["At ZFS 6, factorstokeep: ", factorstokeep];

    strippedequation =
    Part[Flatten[factorswithc], 1] * Product[
        factorstokeep[[k]][[1]] ^ factorstokeep[[k]][[2]],
        {k, 1, Length[factorstokeep]}];
    printZFS["At ZFS 7, strippedequation: ", strippedequation];

    factorscancelled = Factor[givenexpr / strippedequation];
    printZFS["At ZFS 8, factorscancelled: ", factorscancelled],

  (* else Head is not Times *)
    strippedequation = givenexpr
  ]; (* end If Head[givenexpr] *)

  (* Clear all local variables not being returned.                      *)
  Clear[faclist, factorsfreeofc, factorswithc, factorstokeep, factorstocancel,
    factorscancelled, freeofc, notfreeofc, givenexpr];

  Return[strippedequation]
]; (* end of Module zeroFactorStripper *)

(* ##########       Mini-function: freeRhoConstants        ########## *)

(**********************************************************************)
(* freeRhoConstants[currentRho, rankDen]                              *)
(* Purpose: To take the results from the solver and checks to see if  *)
(*          c[i] remain.  If so, it checks for c[i] common to all     *)
(*          terms.  c[i] common to all terms are set to 1.  A note    *)
(*          is given when this is done                                *)
(* Input:   The current density                                       *)
(*          The rank of the density                                   *)
(* Output:  List of free constants                                    *)
(*          Modified density if only one free constant was found      *)
(* Created: 21 August, 2006 by DP at home                             *)
(* Code is in File:  conservationlaws/nwevarh4.m                      *)
(* Last Modified:  13 October, 2009, 9:32 by DP at CSM                *)
(**********************************************************************)

(* This function takes the results from the solver and checks to see if *)
(* c[i] remain.  If so, it checks for c[i] common to all terms.  c[i]   *)
(* common to all terms are set to 1.  A note is given when this is done.*)

freeRhoConstants[currentRho_, rankDen_] :=
Module[{freeConstants, freeRules, fullTermCheck, noOfFreeConstants, newRho,
    printFRC},

  If[debugFreeRhoConstants, printFRC = Print, Clear[printFRC],
      Clear[printFRC]];

  printFRC["debug code: FRC, Function: freeRhoConstants, File: nwevarho.m"];

  printFRC["At FRC IN, the current density being evaluated:"];
  printFRC[currentRho];

  freeConstants = Union[Cases[currentRho, _c, {0, Infinity}]];
  noOfFreeConstants = Length[freeConstants];
  printFRC["At FRC 1, there are ", noOfFreeConstants, " free constants in"<>
      " the current density:"];
  printFRC[freeConstants];

  (* Check for a single free constant common to all terms.  If one      *)
  (* exists, set it equal to 1.  Otherwise do nothing.                  *)
  If[Length[freeConstants] === 1,
    fullTermCheck = If[Head[currentRho] === Plus,
      Map[FreeQ[#, Part[freeConstants, 1]] &, Apply[List, currentRho]],
      {FreeQ[currentRho, Part[freeConstants, 1]]}];
    printFRC["At FRC 2, False indicates the single free constant is found"<>
        " in the corresponding term of rho:"];
    printFRC[fullTermCheck];

    If[Union[fullTermCheck] === {False},
      freeRules = Rule[Part[freeConstants, 1], 1];
      printLOW["There is a common (free) factor in the density"];
      printLOW["  ", pdeform[currentRho]];
      printLOW["The common factor will be set to 1:  Setting ", freeRules, "."];
      newRho = currentRho /. freeRules;
      printLOW["++++++++++++++++++++++++++++++++++++++++++"];
      printLOW["A density for rank ", rankDen, " is"];
      printLOW[pdeform[newRho]];
      printLOW["Note:  The density given here may undergo a normalization"<>
          " or transformation before being reported in the final results."];
      printLOW["++++++++++++++++++++++++++++++++++++++++++"];

      printFRC["At FRC 3, after applying ", freeRules, " the density has"<>
          " changed to:"];
      printFRC[newRho];

      freeConstants = {};
      printFRC["At FRC 4, the list of free constants has changed to:"];
      printFRC[freeConstants],

    (* else *)
      freeRules = {};
      newRho = currentRho
    ], (* end If Union[fulltermCheck] *)

  (* else *)
    freeRules = {};
    newRho = currentRho
  ]; (* end If Length[freeConstants] *)

  printFRC["At FRC OUT, a list of free constants:"];
  printFRC[freeConstants];
  printFRC["At FRC OUT, the density (a common c[i] may have been set to 1:"];
  printFRC[newRho];

  (* Clear all local variables not being returned.                      *)
  Clear[freeRules, fullTermCheck, noOfFreeConstants, printFRC];

  Return[{freeConstants, newRho}]
]; (* end Module freeRhoConstants *)

(* ##########     Mini-function: removeDuplicateTerms      ########## *)

(**********************************************************************)
(* removeDuplicateTerms[incomingList]                                 *)
(* Purpose: To compare a list of expressions and replaces any         *)
(*          expressions with a 0. The position of each expression in  *)
(*          the list is maintained.                                   *)
(* Input:   A list of expressions                                     *)
(* Output:  A list of expressions with all duplicates removed         *)
(* Created: 21 August, 2006 by DP at home                             *)
(* Code is in File:  conservationlaws/nwevarh4.m                      *)
(* Last Modified:  30 April, 2008, 10:41 by DP at CSM                 *)
(**********************************************************************)

removeDuplicateTerms[incomingList_List] :=
Module[{workingList, finalRhoList = {}, compareRho, printRDT},

  If[debugRemoveDuplicateTerms, printRDT = Print, Clear[printRDT],
      Clear[printRDT]];

  printRDT["debug code: RDT, Function: removeDuplicateTerms,"<>
      " File: nwevarh3.m"];

  printRDT["At RDT IN, a list of expressions to be compared:"];
  printRDT[incomingList];

  workingList = incomingList;
  While[workingList =!= {},

    compareRho = First[workingList];
    printRDT["At RDT 1, the expression for comparisson is:"];
    printRDT[compareRho];

    workingList = DeleteCases[workingList, compareRho];
    printRDT["At RDT 2, the list of expressions after all expressions"<>
        " equal to comparisson have been removed:"];
    printRDT[workingList];

    finalRhoList = Append[finalRhoList, compareRho];
    printRDT["At RDT 3, the list in which all duplicated have been removed:"];
    printRDT[finalRhoList]
  ]; (* end While *)

  printRDT["At RDT OUT, the list of expressions with duplicated removed:"];
  printRDT[finalRhoList];

  (* Clear all local variables not being returned.                      *)
  Clear[workingList, compareRho, printRDT];

  Return[finalRhoList]
]; (* end Module removeDuplicateTerms *)

(* ##########            Function: evaluateRho            ########### *)

(**********************************************************************)
(* evaluateRho[currentRhoList, parameters, weightedParameters,        *)
(*     compRulesList, numDepVars, indVarsList, rankOfDen]             *)
(* Purpose: To take a list of rhos from the general solver and        *)
(*          exclude any duplicates or transform rhos that are similar *)
(*          into a final form. Compatibility conditions are also      *)
(*          checked so that rhos created under certain compatibility  *)
(*          conditions are listed separately.                         *)
(* Input:   A list of all forms for a candidate density computed by   *)
(*              the general solver                                    *)
(*          A list of nonweighted parameters                          *)
(*          A list of weighted parameters                             *)
(*          A list of compatibility rules corresponding to each form  *)
(*              of the candidate given by the general solver          *)
(*          The number of dependent variables                         *)
(*          A list of independent variables                           *)
(*          The rank of the density                                   *)
(* Output:  A list of independent densities with their corresponding  *)
(*              compatibility conditions that can be formed from a    *)
(*              single candidate density.  The densities are given in *)
(*              their final form.                                     *)
(* Created: 21 August, 2006 by DP at home                             *)
(* Code is in File:  conservationlaws/nwevarh4.m                      *)
(* Last Modified: 13 October, 2009, 15:44 by DP at CSM                *)
(**********************************************************************)

evaluateRho[currentRhoList_, parameters_, weightedParameters_,
            compRulesList_, numDepVars_, indVarsList_, rankOfDen_] :=
Module[{lengthRhoList = Length[currentRhoList], expectedZeroResult,
    freeConstantList, newRhoList, linkedRhoList, lengthLinked, splitRhos,
    sr, finalRhoList, numIndVars = Length[indVarsList], variDerResult,
    positionZeros, i1, newCompRulesList, normalRhoList, n1, printER},

  If[debugEvaluateRho, printER = Print, Clear[printER], Clear[printER]];

  printER["debug code: ER, Function: evaluateRho, File: nwevarho.m"];

  printER["At ER IN, a list of densities from the general solver:"];
  printER[currentRhoList];
  printER["At ER IN, the current density list contains ", lengthRhoList,
      " term(s)."];
  printER["At ER IN, a list of compatibility rules corresponding to each"<>
      " density in the list of densities:"];
  printER[compRulesList];
  printER["At ER IN, a list of all parameters contains ", parameters];
  printER["At ER IN, a list of all weighted parameters contains ",
      weightedParameters];
  printER["At ER IN, there are ", numDepVars, " dependent variables."];
  printER["At ER IN, the independent variables are: ", indVarsList];
  printER["At ER IN, the rank of the densit"<>
      If[Length[currentRhoList] === 1, "y", "ies"]<>" is ", rankOfDen];

  expectedZeroResult = Table[0, {i, 1, numDepVars}];

  (* Find any free constants in each rho expression.  If there is a     *)
  (* single free constant common to all terms, create a rule mapping    *)
  (* that constant to 1. A list of free constants is provided in any    *)
  (* case.                                                              *)
  freeConstantList = Map[freeRhoConstants[#, rankOfDen] &, currentRhoList];

  newRhoList = Map[Part[#, 2] &, freeConstantList];
  printER["At ER 1, after being evaluated by freeConstants, the list of"<>
      " rhos now contains:"];
  printER[newRhoList];

  freeConstantList = Map[Part[#, 1] &, freeConstantList];
  printER["At ER 2, a list of free constants corresponding to the list"<>
      " of rhos:"];
  printER[freeConstantList];

  (* Compare all rhos calculated during compatibility condition check   *)
  (* and determine whether they are the same or different.              *)

  If[lengthRhoList > 1,
    (* First check for any free constants that cannot be removed.       *)
    (* Expressions with free constants may be split into pieces with    *)
    (* each piece forming a density.                                    *)
    If[MemberQ[freeConstantList, _c, Infinity],

      printER["At ER 3, some c[i] exist in the rhos.  The algorithm will"<>
          " attempt to split rho into pieces organized around the c[i]."];

      (* Link rhos to their compatibility conditions.                   *)
      linkedRhoList =
          Thread[List[newRhoList, compRulesList, freeConstantList]];
      printER["At ER 4, after threading the rho list with the compatibility"<>
          " list and the free constant list:"];
      printER[linkedRhoList];
      printER["At ER 4a, there are ", Length[linkedRhoList], " expressions"<>
          " for rho in this list."];

      (* Remove any duplicate expressions already in place. Keep track  *)
      (* of the compatibility conditions and the free constants.        *)
      linkedRhoList = removeDuplicateTerms[linkedRhoList];
      printER["At ER 5, the list of normalized terms with duplicates"<>
          " removed:"];
      printER[linkedRhoList];
      lengthLinked = Length[linkedRhoList];
      printER["At ER 5a, the number of normalized terms is now"<>
          " ", lengthLinked];

      printHIGH["------------------------------------------"];
      printHIGH["Several solutions to the system of undetermined"<>
          " coefficients exist, producing several forms of the candidate"<>
          " density.  Each form of the density containins free coefficients,"<>
          " which indicate that the candidate consists of a linear"<>
          " combination of densities.  Each density will be split"<>
          " according to its coefficient as follows."];

      (* Split the rho by separating terms according to which c[i]      *)
      (* they are attached to.                                          *)
      splitRhos = Thread[sr[linkedRhoList,
          Table[weightedParameters, {i, 1, lengthLinked}],
          Table[numDepVars, {i, 1, lengthLinked}],
          Table[indVarsList, {i, 1, lengthLinked}],
          Table[rankOfDen, {i, 1, lengthLinked}]]] /.
          sr -> separateRhos;
      printER["At ER 6, after running all different rhos through separate"<>
          " rhos:"];
      printER[splitRhos];
      printER["At ER 6a, there are ", Length[splitRhos], " groups of"<>
          " split densities in the list."];

      (* Eliminate all duplicates.                                      *)
      splitRhos = Flatten[splitRhos, 1];
      splitRhos = removeDuplicateTerms[splitRhos];
      printER["At ER 7, after removing all duplicate expressions, the split"<>
          " density list now contains:"];
      printER[splitRhos];
      printER["At ER 7a, there are ", Length[splitRhos], " expressions in"<>
          " the reduced list of split densities."];

      (* Check all remaining rhos for independence.                     *)
      If[splitRhos =!= {{}},
        finalRhoList = indepDriver[splitRhos, indVarsList, numDepVars,
            weightedParameters];
        printER["At ER 8, after removing all dependent densities, the"<>
            " density list now contains:"];
        printER[finalRhoList];
        printER["At ER 8a, there are ", Length[finalRhoList], " expressions in"<>
            " the reduced list of split densities."],

      (* else *)
        finalRhoList = {{}, {}}
      ], (* end If splitRhos *)

    (* else *)
      printER["At ER 10, there are no c[i] in any of the rhos.  The"<>
          " algorithm will first normalize, then remove duplicate rhos"<>
          " from the list."];

      (* check to see if the rho has a zero variational derivative.     *)
      variDerResult = Map[Table[variationalDerivativeMultiD[#, i, indVarsList],
          {i, 1, numDepVars}]&, newRhoList];
      printER["At ER 11, the variational derivative check yields:"];
      printER[variDerResult];

      positionZeros = Position[variDerResult, Table[0, {i, 1, numDepVars}]];
      printER["At ER 12, positions where zero variational derivatives exist:"];
      printER[positionZeros];
      For[i1 = 1, i1 <= Length[positionZeros], i1++,
        Print["-------------------------------------------"];
        Print["A density has been calculated to be"];
        Print[pdeform[Part[newRhoList, Part[Flatten[positionZeros], i1]]]];
        Print["However, after applying the variational derivative to the"<>
            " density, the result is ", expectedZeroResult, ",", " so"<>
            " the density is removed since it can be incorporated into"<>
            " the flux."]
      ]; (* end For *)
      newRhoList = Delete[newRhoList, positionZeros];
      newCompRulesList = Delete[compRulesList, positionZeros];
      printER["At ER 13, the after removing zero variational derivative"<>
          " rhos, the rho list contains"];
      printER[newRhoList];

      (* Normalize all expressions.                                     *)
      normalRhoList = Map[newNormalize[#, weightedParameters] &,
          newRhoList];
      printER["At ER 14, All expression in the current list have been"<>
          " normalized:"];
      printER[normalRhoList];

      (* Match rhos to their compatibility conditions.                  *)
      normalRhoList = Thread[List[normalRhoList, newCompRulesList]];

      (* Remove duplicate expressions.                                  *)
      finalRhoList = removeDuplicateTerms[normalRhoList];
      printER["At ER 15, Duplicate terms have been removed from the list"<>
          " of terms without c[i]:"];
      printER[finalRhoList]
    ], (* end If MemberQ *)
  (* else *)

    (* Check for terms with free c[i].  If they exist, split the rho    *)
    (* into parts.                                                      *)
    If[MemberQ[newRhoList, _c, Infinity],
      printER["At ER 16, A single expression for rho contains free c[i]."<>
          " The expression will be split into parts."];

      finalRhoList = separateRhos[{newRhoList, compRulesList,
          Flatten[freeConstantList]}, weightedParameters, numDepVars,
          indVarsList, rankOfDen];
      printER["At ER 17, a list of rhos with their compatibility conditions"<>
          " after being split according to free c[i]:"];
      printER[finalRhoList],

    (* else *)
      printER["At ER 18, only one rho expression exists. It will be"<>
          " normalized."];

      newRhoList = newRhoList /. List[n1__]-> n1;
      printER["At ER 19, the single expression has been removed from being"<>
          " inside List:"];
      printER[newRhoList];

      (* If rho = 0 because constants vanish, change zero to empty list *)
      (* indicating rho does not exist.                                 *)
      If[newRhoList === 0, newRhoList = {}];
      printER["At ER 20 the single expression after checking for zero:"];
      printER[newRhoList];

      (* If an expression for rho exists, normalize it.                 *)
      If[newRhoList =!= {},
        (* Check the variationalDerivative.                             *)
        variDerResult = Table[variationalDerivativeMultiD[
            newRhoList, i, indVarsList], {i, 1, numDepVars}];
        If[variDerResult === expectedZeroResult,
          Print["------------------------------------------"];
          Print["A density has been calculated to be"];
          Print[pdeform[newRhoList]];
          Print["However, after applying the variational derivative to the"<>
              " density, the result is ", expectedZeroResult, ",", " so"<>
              " the density is removed since it can be incorporated into"<>
              " the flux."]
        ]; (* end If variDerResult *)
        newRhoList = If[variDerResult === expectedZeroResult,
            0, newRhoList, newRhoList];
        normalRhoList = newNormalize[newRhoList, weightedParameters];
        finalRhoList = If[normalRhoList === 0,
            {{},{}}, {{normalRhoList, compRulesList}}],

      (* else *)
        finalRhoList = {{}, {}}
      ] (* end If newRhoList *)
    ]; (* end If MemberQ *)

    printER["At ER 21, the normalized finalRhoList with its compatibility"<>
        " condition:"];
    printER[finalRhoList]
  ]; (* end If lengthRhoList *)

  printER["At ER OUT, final densities paired with their compatibility"<>
      " conditions:"];
  printER[finalRhoList];

  (* Clear all local variables not being returned.                      *)
  Clear[lengthRhoList, expectedZeroResult, freeConstantList, newRhoList,
      linkedRhoList, lengthLinked, splitRhos, sr, numIndVars, variDerResult,
      positionZeros, i1, newCompRulesList, normalRhoList, n1, printER];

  Return[finalRhoList]
]; (* end Module evaluateRho *)

(* ##########            Function: newNormalize            ########## *)

(**********************************************************************)
(* newNormalize[expression, wtdParams, outCoef]                       *)
(* Purpose: To find the highest degree term, set its coefficient to   *)
(*          one and adjust all other coefficients accordingly.        *)
(* Input:   An expression.                                            *)
(*          A list of weighted parameters                             *)
(*          An indicator as to whether to output the normalized       *)
(*               coefficient                                          *)
(* Output:  An expression in normalized form.                         *)
(* Created: 11 January, 2007 by DP at CSM                             *)
(* Code is in File:  conservationlaws/nwnewnor.m                      *)
(* Last Modified:  2 February, 2010, 13:11 by DP at CSM               *)
(**********************************************************************)

newNormalize[expression_, wtdParams_List, outCoef_:0] :=
Module[{expandedExpression, strippedList, strippedListAfterLog, exponentList,
    positionHighestTerms, leadingTermFull, leadingTerm, leadingTermSimple,
    leadingTermPosition, leadingCoefficient, n1, n2, variableExp, numForVar,
    printNN},

  If[debugNewNormalize, printNN = Print, Clear[printNN], Clear[printNN]];

  printNN["debug code: NN, Function: newNormalize, File: nwnewnor.m"];

  printNN["At NN IN, the expression input is:"];
  printNN[expression];
  printNN["At NN IN, a list of weighted parameters contains ", wtdParams];
  printNN["At NN IN, if outCoef is 0, the leading coefficient is not"<>
      " returned:  outCoef = ", outCoef];

  (* If given expression is 0, normalization is not necessary.          *)
  If[expression === 0, Return[If[outCoef =!= 0, {0,1}, 0]]];

  expandedExpression = Expand[expression];

  (* Strip all coefficients and parameters off to determine the         *)
  (* highest degree term.                                               *)
  strippedList = stripper[expandedExpression];
  printNN["At NN 1, a stripped list of terms in the expression:"];
  printNN[strippedList];

  (* Find the degree of each term.                                      *)
  strippedListAfterLog = PowerExpand[Log[strippedList]];
  printNN["At NN 2, the stripped list of terms after Log and PowerExpand"<>
      " have been applied:"];
  printNN[strippedListAfterLog];

  exponentList = strippedListAfterLog /.{Log[u[_][__]] -> 1,
      Log[Derivative[__][u[_]][__]] -> 1};
  printNN["At NN 3, the log of u and derivatives of u are changed to 1:"];
  printNN[exponentList];

  exponentList = exponentList /. Log[__] -> 0;
  printNN["At NN 3a, the log of other functions is changed to zero:"];
  printNN[exponentList];

  exponentList = exponentList /. {t -> 0, x -> 0, y -> 0, z -> 0};
  printNN["At NN 3b, independent variables have been removed from the"<>
      " expression:"];
  printNN[exponentList];

  If[Union[Map[NumericQ, exponentList]] =!= {True},
    variableExp = Map[Rule[#, 1000] &, Union[exponentList /. _Integer -> 0]];
    printNN["At NN 3a, exponents that are variables:"];
    printNN[variableExp];

    numForVar = exponentList /. variableExp;
    printNN["At NN 3b, variables in the list have been replaced by numbers:"];
    printNN[numForVar];

    positionHighestTerms = Take[Position[numForVar, Max[numForVar]], 1];
    printNN["At NN 3c, the variable taken as the maximum (there may be"<>
        " more than one:"];
    printNN[positionHighestTerms],

  (* else *)

    (* Determine the highest degree term(s).  There may be several terms  *)
    (* with the same degree.                                              *)
    positionHighestTerms = Position[exponentList, Max[exponentList]];
    printNN["At NN 4, the highest degree terms occur in these positions:"];
    printNN[positionHighestTerms]
  ]; (* end If Union Map NumericQ *)

  leadingTermFull = If[Head[expandedExpression] === Plus,
    Part[expandedExpression, Sequence[Flatten[positionHighestTerms]]],
    expandedExpression];
  printNN["At NN 5, possible leading terms include (there may be more"<>
      " than one here):"];
  printNN[leadingTermFull];

  (* If there is more than one term with the highest degree, pick the   *)
  (* one with the largest exponent first, then choose the one with the  *)
  (* lowest numbered dependent variable. *)
  If[Length[positionHighestTerms] > 1,

    (* Used stripped terms to make determinations.                       *)
    leadingTerm = Part[strippedList, Sequence[Flatten[positionHighestTerms]]];
    printNN["At NN 6, a stripped list of the leading terms:"];
    printNN[leadingTerm];

    (* Isolate terms with the highest exponents.                        *)
    leadingTermSimple = Delete[leadingTerm, Position[leadingTerm, _Times]];
    If[leadingTermSimple === {},
      leadingTerm = Part[leadingTerm, 1],
    (* else *)
      leadingTerm = leadingTermSimple
    ]; (* end If leadingTermSimple *)
    printNN["At NN 7, a stripped list after terms with head Times have"<>
        " been removed:"];
    printNN[leadingTerm];

    (* Apply Plus to order the dependent variables on terms that are    *)
    (* left.                                                            *)
    If[Head[leadingTerm] =!= Times, leadingTerm = Apply[Plus, leadingTerm]];
    leadingTerm = stripper[leadingTerm];
    printNN["At NN 8, a stripped list after terms have been reordered:"];
    printNN[leadingTerm];

    (* The first term should be the one that is needed.                 *)
    leadingTerm = Part[leadingTerm, 1];
    printNN["At NN 9, the leading term (term with highest degree and"<>
        " largest exponent) contains:"];
    printNN[leadingTerm];

    leadingTermPosition =
        If[Head[leadingTerm] === Times,
          Position[expandedExpression, Times[___, leadingTerm]],
        (* else *)
          Position[expandedExpression, leadingTerm]
        ]; (* end If Head *)

    leadingTermPosition = Map[Part[#, 1] &, leadingTermPosition];
    printNN["At NN 10, the position of the actual leading term is "<>
        "", leadingTermPosition];

    (* The leading term with all coefficients is taken from the list    *)
    (* of terms.                                                        *)
    leadingTermFull = Apply[Plus,
        Extract[expandedExpression, Transpose[{leadingTermPosition}]]];
    printNN["At NN 11, the leading term is ", leadingTermFull],

  (* else *)
    (* Take the stripped leading term from the list of stripped terms.  *)
    leadingTerm = Part[Extract[strippedList, positionHighestTerms], 1];
    printNN["At NN 12, the stripped leading term is ", leadingTerm]
  ]; (* end If leadingTerm *)

  (* Determine the coefficient on the leading term.                     *)
  leadingCoefficient = Together[leadingTermFull/leadingTerm];
    printNN["At NN 13, the coefficient on the leading term is "<>
        "", leadingCoefficient];

  (* If the leading coefficient has independent variables in it, end    *)
  (* the normalizaion.                                                  *)
  If[MemberQ[leadingCoefficient, Alternatives[t,x,y,z], {0, Infinity}],
    If[outCoef === 0,
      Return[expandedExpression],
      Return[{expandedExpression, leadingCoefficient}]]
  ] (* end If MemberQ *);

  (* Normalize the expression by dividing all terms in the given        *)
  (* expression by the coefficient on the leading term.                 *)
  expandedExpression = Expand[MapAll[Apart,
      expandedExpression/leadingCoefficient]];
  printNN["At NN 14, the expression after dividing by the leading"<>
      " coefficient:"];
  printNN[expandedExpression];

  (* The next two commands reorganize the c[i] so that the c[i] can be  *)
  (* written in a linear form, allowing a linear system of equations to *)
  (* be formed. *)
  If[MemberQ[leadingCoefficient, _c, Infinity],

    (* Reset any terms that that have c[i]/c[j] to c[i].  Since each    *)
    (* term contains only one c[i], the terms can still be gathered     *)
    (* according to different constants, even if there are several      *)
    (*  different c[i].                                                 *)
    expandedExpression = expandedExpression /. c[n1_]/c[n2_] -> c[n1];
    printNN["At NN 15, the normalized expression with all c[i]/c[j] renamed"<>
        " to c[i]:"];
    printNN[expandedExpression];

    (* Move any c[i] in denominators back to the numerator.             *)
    expandedExpression = expandedExpression /. Power[c[n1_], -1] -> c[n1];
    printNN["At NN 16, the normalized expression with all c[i] moved to the"<>
        " numerator:"];
    printNN[expandedExpression]
  ]; (* end If MemberQ *)

  (* Simplify the expression if variables exist in the denominator.     *)
  If[MemberQ[expandedExpression, Power[__, -1], Infinity],
    expandedExpression = expressionSimplify[expandedExpression]
  ]; (* end If MemberQ *)

  printNN["At NN OUT, the expression after normalization:"];
  printNN[expandedExpression];

  (* Clear all local variables not being returned.                      *)
  Clear[strippedList, strippedListAfterLog, exponentList, positionHighestTerms,
      leadingTermFull, leadingTerm, leadingTermSimple, leadingTermPosition, n1,
      n2, variableExp, numForVar, printNN];

  If[outCoef === 0,
    Return[expandedExpression],
    Return[{expandedExpression, leadingCoefficient}]];
]; (* end Module newNormalize *)

(* ##########         Function: expressionSimplify         ########## *)

(**********************************************************************)
(* expressionSimplify[expression]                                     *)
(* Purpose: To combine terms which have the same form of u[_]. This   *)
(*          function will most likely be used when terms have         *)
(*          parameters in denominators and Expand does not properly   *)
(*          combine like terms.                                       *)
(* Input:   an expression.                                            *)
(* Output:  a simplified expression.                                  *)
(* Created: 12 January, 2007 by DP at CSM                             *)
(* Code is in File:  conservationlaws/nwexpsim.m                      *)
(* Last Modified:  30 April, 2008, 11:53 by DP at CSM                 *)
(**********************************************************************)

expressionSimplify[expression_] :=
Module[{strippedExpn, initialExpn, expnCompare, positionLikeTerms, likeTerms,
    simpleExpn = {}, printES},

  If[debugExpressionSimplify, printES = Print, Clear[printES], Clear[printES]];

  printES["debug code: ES, Function: expressionSimplify, File: nwexpsim.m"];

  printES["At ES IN, the expression to be simplified:"];
  printES[expression];

  (* Create a list of stripped terms to provide bases for identifying   *)
  (* and combining like terms.                                          *)
  strippedExpn = stripper[expression];
  printES["At ES 1, a stripped list of terms in the expression:"];
  printES[strippedExpn];

  (* Write the expression into a list of terms.                         *)
  initialExpn = If[Head[expression] === Plus, Apply[List, expression],
      {expression}, {expression}];
  printES["At ES 2, the initial expression after List has been applied:"];
  printES[initialExpn];

  (* Pick off the first term, then identify all terms with the same     *)
  (* base.  Once they are grouped, apply Factor and Expand to get any   *)
  (* common factors to cancel out.  Plus does not always do this when   *)
  (* variables exist in denominators.                                   *)
  While[initialExpn =!= {},
    expnCompare = Part[strippedExpn, 1];
    printES["At ES 3, the current term up for combination: ", expnCompare];
    positionLikeTerms = Position[strippedExpn, expnCompare,{1}];
    printES["At ES 4, positions of all like terms: ", positionLikeTerms];

    (* This is the combination step.                                    *)
    If[Length[positionLikeTerms] > 1,
      likeTerms = {Factor[Expand[Apply[Plus,
          Extract[initialExpn, positionLikeTerms]]]]};
      printES["At ES 5, the combination of like terms after applying Factor"<>
          " and Expand: ", likeTerms],

    (* else *)
      likeTerms = Together[Extract[initialExpn, positionLikeTerms]];
      printES["At ES 5a, the term stands alone: ", likeTerms]
    ]; (* end If Length *)

    (* Combine simplified terms with others already simplified.         *)
    simpleExpn = Union[simpleExpn, likeTerms];
    printES["At ES 6, the simplified expression now has these terms:"];
    printES[simpleExpn];

    (* Adjust the lists.                                                *)
    initialExpn = Delete[initialExpn, positionLikeTerms];
    strippedExpn = Delete[strippedExpn, positionLikeTerms];
    printES["At ES 7, the expression now contains:"];
    printES[initialExpn];
    printES["At ES 8, the stripped list of terms now contains:"];
    printES[strippedExpn]
  ]; (* end While *)

  simpleExpn = Apply[Plus, simpleExpn];

  printES["At ES OUT, the simplified expression:"];
  printES[simpleExpn];

  (* Clear all local variables not being returned.                      *)
  Clear[strippedExpn, initialExpn, expnCompare, positionLikeTerms, likeTerms,
      printES];

  Return[simpleExpn]
]; (* end Module expressionSimplify *)

(* ##########            Function: separateRhos            ########## *)

(**********************************************************************)
(* separateRhos[groupedList, weightParam, numDepVars, IndVarList,     *)
(*     rankOfDen]                                                     *)
(* Purpose: To form smaller rhos from a large expression by picking   *)
(*          off terms associated with different c[i] and terms with   *)
(*          no c[i].                                                  *)
(* Input:   A list which groups a density with its compatibility      *)
(*              conditions and its free c[i]                          *)
(*          A list of weighted parameters                             *)
(*          The number of dependent variables                         *)
(*          A list of independent variables                           *)
(*          The rank of the density                                   *)
(* Output:  A list of densities paired with their compatibility       *)
(*              conditions which have been formed according to the    *)
(*              free c[i]                                             *)
(* Created: 25 January, 2007 by DP at CSM                             *)
(* Code is in File:  conservationlaws/nwseprho.m                      *)
(* Last Modified:  13 October, 2009, 11:19 by DP at CSM               *)
(**********************************************************************)

separateRhos[groupedList_, weightParam_, numDepVars_, IndVarList_,
    rankOfDen_] :=
Module[{expandedRho, makeSeparateRhos, lastRho, varDerResult, positionZeros,
    expectedZeroResult, freeConst, i1, elimFree, numSplitRhos, printSR},

  If[debugSeparateRhos, printSR = Print, Clear[printSR], Clear[printSR]];

  printSR["debug code: SR, Function: separateRhos, File: nwseprho.m"];

  printSR["At SR IN, the current density with multiple c[i]:"];
  printSR[Part[groupedList, 1]];
  printSR["At SR IN, free constants in this density are ",
      Part[groupedList, 3]];
  printSR["At SR IN, compatibility conditions for this density are:"];
  printSR[Part[groupedList, 2]];
  printSR["At SR IN, the weighted parameters in this density are: ",
      weightParam];
  printSR["At SR IN, there are ", numDepVars, " dependent variables."];
  printSR["At SR IN, the independent variables are: ", IndVarList];
  printSR["At SR IN, the rank of the density is: ", rankOfDen];

  If[Part[groupedList, 3] === {},
    Return[{{Part[groupedList, 1], Part[groupedList, 2]}}]
  ]; (* end If Part[groupedList, 3] *)

  expandedRho = Expand[Factor[Part[groupedList, 1]]];
  printSR["At SR 1, the current rho in expanded form:"];
  printSR[expandedRho];

  (* Pick out terms with c[i]s and place them into separate lists for   *)
  (* each c[i].                                                         *)
  makeSeparateRhos = Map[Cases[expandedRho, Times[__, #], Infinity] &,
      Part[groupedList, 3]];
  makeSeparateRhos = Apply[Plus, makeSeparateRhos, {1}];
  printSR["At SR 2, a list of expressions formed with each c[i] as a factor:"];
  printSR[makeSeparateRhos];

  (* The last expression is formed from all terms that don't have any   *)
  (* c[i].                                                              *)
  lastRho = Apply[Plus, expandedRho] - Apply[Plus, makeSeparateRhos];
  printSR["At SR 3, the part of rho not containing any c[i]:"];
  printSR[lastRho];

  freeConst = If[lastRho === 0, Part[groupedList, 3],
      Append[Part[groupedList, 3], {}]];
  printSR["At SR 4, the free constant list has been adjusted when a density"<>
      " exists without a c[i]:"];
  printSR[freeConst];

  (* Make a list containing all of the splits.                          *)
  makeSeparateRhos = If[lastRho =!= 0,
      Append[makeSeparateRhos, lastRho], makeSeparateRhos];
  makeSeparateRhos = makeSeparateRhos /. c[_] -> 1;
  printSR["At SR 5, a list of rhos split off of the original expression:"];
  printSR[makeSeparateRhos];

  (* Run all newly formed rhos through the variational derivative.      *)
  (* If the result contains all zeros, discard the rho.                 *)
  varDerResult = Map[Table[variationalDerivativeMultiD[#, i1, IndVarList],
      {i1, 1, numDepVars}]&, makeSeparateRhos];
  printSR["At SR 6, the result after applying the variational derivative"<>
      " to each rho:"];
  printSR[varDerResult];

  expectedZeroResult = Table[0, {i1, 1, numDepVars}];
  positionZeros = Position[varDerResult, expectedZeroResult];
  printSR["At SR 7, the positions of rhos with zero variational"<>
      " derivatives:"];
  printSR[positionZeros];

  makeSeparateRhos = Delete[makeSeparateRhos, positionZeros];
  printSR["At SR 8, the list of separated rhos after removal of rhos with"<>
      " zero variational derivatives:"];
  printSR[makeSeparateRhos];

  freeConst = Flatten[Delete[freeConst, positionZeros]];
  printSR["At SR 9, free constants remainaing are:"];
  printSR[freeConst];

  elimFree = Complement[Part[groupedList, 3], freeConst];
  printSR["At SR 10, free constants eliminated are:"];
  printSR[elimFree];

  (* Normalize each of the expressions. This will also remove any       *)
  (* common factors.                                                    *)
  makeSeparateRhos = Map[newNormalize[#, weightParam] &, makeSeparateRhos];
  printSR["At SR 11, a list of rhos split off of the original expression"<>
      " after normalization:"];
  printSR[makeSeparateRhos];

  numSplitRhos = Length[makeSeparateRhos];
  printSR["At SR 12, there are ", numSplitRhos, " split densities."];

  (* SUMMARY *)
  If[makeSeparateRhos =!= {},
    printHIGH["------------------------------------------"];
    printHIGH["A density computed for rank ", rankOfDen, " has two or more"<>
        " free constants:"];
    printHIGH[pdeform[expandedRho], "\n"];
    If[elimFree =!= {},
      printHIGH["Candidates with free constants ", elimFree, " have been"<>
          " eliminated since they have zero variationalDerivatives."]
    ]; (* end If elimFree *)

    printHIGH["This density is a linear combination independent densities."<>
        "  The density will be split into its independent parts by"<>
        " grouping terms according to the free constants:"];
    printHIGH[freeConst];
    If[lastRho === 0,
      For[i1 = 1,  i1 <= numSplitRhos, i1++,
        printHIGH["The part of the density of rank ", rankOfDen, " with"<>
            " free constant ", Part[freeConst, i1], " is"];
        printHIGH[pdeform[Part[makeSeparateRhos, i1]], "\n"]
      ], (* end For *)
    (* else *)
      For[i1 = 1,  i1 < numSplitRhos, i1++,
        printHIGH["The part of the density of rank ", rankOfDen, " with"<>
            " free constant ", Part[freeConst, i1], " is"];
        printHIGH[pdeform[Part[makeSeparateRhos, i1]], "\n"]
      ]; (* end For *)
      printHIGH["The part of the density that has no free constants is"];
      printHIGH[pdeform[Part[makeSeparateRhos, numSplitRhos]], "\n"]
    ]; (* end If lastRho *)

  (* Match smaller rhos to the compatibility condition. *)

    makeSeparateRhos = Thread[List[makeSeparateRhos,
        Table[Part[groupedList, 2], {i1, 1, numSplitRhos}]]],

  (* else *)
    makeSeparateRhos = {{},{}}
  ]; (* end If makeSerarateRhos *)

  printSR["At SR OUT, a list of rhos paired with their compatibility"<>
      " conditions:"];
  printSR[makeSeparateRhos];

  (* Clear all local variables not being returned.                      *)
  Clear[expandedRho, lastRho, varDerResult, positionZeros, numSplitRhos,
    expectedZeroResult, freeConst, i1, elimFree,  printSR];

 Return[makeSeparateRhos]
]; (* end Module separateRhos *)

(* ##########         Function: checkForDivergences        ########## *)

(**********************************************************************)
(* checkForDivergences[density, flux, indepVarsList]                  *)
(* Purpose: When a transformation has been applied to the PDE to put  *)
(*          into evolution form, this function may be nessary after   *)
(*          densities and fluxes have been written back into the form *)
(*          of the orginal PDE.  The function checks for zero and     *)
(*          trivial densities and to removes terms that are           *)
(*          divergences from the densities.                           *)
(* Input:   A density                                                 *)
(*          A flux                                                    *)
(*          A list of independent variables                           *)
(* Output:  Densities and fluxes in their final form                  *)
(* Created: 19 December, 2007 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwchkdiv.m                      *)
(* Last Modified:  9 May, 2008, 12:08 by DP at CSM                    *)
(**********************************************************************)

checkForDivergences[density_, flux_, indepVarsList_] :=
Module[{noOfDepVars, n1, n2, zeroResult, densityList, varDer, posZeroVarDer,
    nonZeroVarDer, constantList, b, newVarDer, strippedVarDer, coefSystem,
    coefSolution, coefSoluprintSRtion, dependentCoefs, rul, partialVarDer,
    termsFormingADerivative, currentTerm, i1, i2, i3, indepVar, printCFDI,
    lthIVL = Length[indepVarsList], termBeingMoved, newFlux = flux},

  If[debugCheckForDivergences, printCFDI = Print, Clear[printCFDI],
      Clear[printCFDI]];

  printCFDI["debug code: CFDI, Function: checkForDivergences,"<>
      " File: nwchkdiv.m"];

  printCFDI["At CFDI IN, the density given:"];
  printCFDI[density];
  printCFDI["At CFDI IN, the corresponding flux given:"];
  printCFDI[flux];
  printCFDI["At CFDI IN, the independent variable list is ", indepVarsList];

  noOfDepVars = If[density === 0, 1, Max[Union[Cases[density,
      Derivative[__][u[_]][__], {0, Infinity}],
      Cases[density, _u[__], {0, Infinity}]] /.
     {Derivative[__][u[n1_]][__] -> n1, u[n2_][__] -> n2}]];
  zeroResult = Table[0, {i1, 1, noOfDepVars}];
  printCFDI["At CFDI 0, there are ", noOfDepVars, " dependent variables"];

  (* Do not report zero densities.                                      *)
  If[density === 0, Return[{{}, {}}]];

  (* Check the variation derivative on the density.                     *)
  densityList = If[Head[density] === Plus, Apply[List, density], {density}];
  varDer = Map[Table[variationalDerivativeMultiD[#, i1, indepVarsList],
      {i1, 1, noOfDepVars}] &, densityList];
  printCFDI["At CFDI 1, the variational derivative mapped across each term"<>
      " in the density produces:"];
  printCFDI[varDer];

  (* If the density is a divergence or total derivative, it will be     *)
  (* reported, but noted as a trivial density.                          *)
  If[Apply[Plus, varDer] === zeroResult, Return[{density, flux}]];

  (* If the density contains a term or group of terms that form a       *)
  (* divergence, the term(s) will be  moved into the flux.              *)
  posZeroVarDer = Position[varDer, zeroResult];
  printCFDI["At CFDI 2, the position of zero variational derivatives in the"<>
      " density list:"];
  printCFDI[posZeroVarDer];

  nonZeroVarDer = Delete[varDer, posZeroVarDer];
  printCFDI["At CFDI 3, all zero variational derivatives have been removed:"];
  printCFDI[nonZeroVarDer];

  (* check for a group of terms that may form a divergence.             *)
  constantList = Table[b[i1], {i1, 1, Length[nonZeroVarDer]}];
  newVarDer = Expand[Thread[Times[constantList, nonZeroVarDer]]];
  printCFDI["At CFDI 4, constants b[i] have been distributed through the terms:"];
  printCFDI[newVarDer];

  strippedVarDer = DeleteCases[Union[Flatten[Map[stripper,
      Flatten[nonZeroVarDer]]]], 0, {0, 1}];
  printCFDI["At CFDI 5, a list of terms found in the variational derivative"<>
      " afprintSRter all coefficients have been stripped:"];
  printCFDI[strippedVarDer];

  newVarDer = Map[Cases[newVarDer, Times[___, b[_], #], {0,Infinity}] &,
      strippedVarDer];
  printCFDI["At CFDI 6, terms in the variational derivative with constants"<>
      " have been grouped according to the list of stripped terms:"];
  printCFDI[newVarDer];

  newVarDer = Expand[Thread[Times[1/strippedVarDer,
      Apply[Plus, newVarDer, {1}]]]];
  printCFDI["At CFDI 7, the dependent variables and their derivatives have"<>
      " been removed from thegroups:"];
  printCFDI[newVarDer];

  coefSystem = Map[Equal[#, 0] &, newVarDer];
  printCFDI["At CFDI 8, a system of coefficient equations is formed:"];
  printCFDI[coefSystem];

  coefSolution = Solve[coefSystem, constantList];
  printCFDI["At CFDI 9, the solution to the system of coefficients:"];
  printCFDI[coefSolution];

  (* Interest is in dependent coefficients only.                        *)
  coefSoluprintSRtion = DeleteCases[Flatten[coefSolution], Rule[_, 0]];
  printCFDI["At CFDI 10, the solution to the system of coefficients after"<>
      " all zero solutions have been removed:"];
  printCFDI[coefSolution];

  dependentCoefs = Union[coefSolution /. Rule[_, n1_] -> n1];
  printCFDI["At CFDI 11, a list of dependent constants on the right-hand"<>
      " side of each solution:"];
  printCFDI[dependentCoefs];

  dependentCoefs = Map[Union, Map[Cases[coefSolution /.
      Rule -> rul, rul[_, #], {0,Infinity}] &, dependentCoefs] /.
      rul -> Sequence];
  printCFDI["At CFDI 12, a list of all dependent constants:"];
  printCFDI[dependentCoefs];

  termsFormingADerivative = Union[Map[Extract[Delete[densityList, posZeroVarDer],
      #] &, dependentCoefs /. b[n1_] -> {n1}],
      Map[List,Extract[densityList, posZeroVarDer]]];
  printCFDI["printSRAt CFDI 13, terms and groups of terms from the density that"<>
      " are divergences and can be moved into the flux:"];
  printCFDI[termsFormingADerivative];

  While[termsFormingADerivative =!= {},
    currentTerm = First[termsFormingADerivative];
    printCFDI["At CFDI 14, the current divergence term from the density:"];
    printCFDI[currentTerm];

    partialVarDer = Table[Map[variationalDerivativeMultiD[#, i2,
        {Part[indepVarsList, i3]}] &, currentTerm],
        {i2, 1, noOfDepVars}, {i3, 1, lthIVL}];
    printCFDI["At CFDI 15, partial variational derivatives are applied to"<>
        " determine which independent variable the derivative is based on:"];
    printCFDI[partialVarDer];

    indepVar = DeleteCases[Apply[Plus, partialVarDer, {2}], zeroResult];
    printCFDI["At CFDI 16, all zero partial variational derivatives are"<>
        " removed:"];
    printCFDI[indepVar];

    indepVar = Position[indepVar, 0, {0,2}];
    indepVar = If[indepVar === {}, Part[indepVarsList, 1], Part[indepVarsList,
        Part[Flatten[Union[Map[Drop[#, 1] &, indepVar]]], 1]]];
    printCFDI["At CFDI 17, the independent variable the derivative is with"<>
        " respect to is ", indepVar];

    termBeingMoved = D[Apply[Plus, currentTerm], t];
    printCFDI["At CFDI 18, the term being moved is (a t-derivative has been"<>
        " applied since the term is being taken out of the density):"];
    printCFDI[termBeingMoved];

    termBeingMoved = Integrate[termBeingMoved, indepVar];
    printCFDI["At CFDI 19, the term to be placed in the ", indepVar, "-"<>
        "component of the flux:"];
    printCFDI[termBeingMoved];

    newFlux = newFlux + Flatten[indepVarsList /.
        {indepVar -> termBeingMoved, x -> 0, y -> 0, z -> 0}];
    printCFDI["At CFDI 20, the revised flux:"];
    printCFDI[newFlux];

    termsFormingADerivative = Drop[termsFormingADerivative, 1]
  ]; (* end While *)

  densityList =  Delete[densityList, posZeroVarDer];
  densityList =  Delete[densityList,
      Apply[List, Flatten[dependentCoefs], {0, 1}]];
  densityList =  {Apply[Plus, densityList]};
  printCFDI["At CFDI 21, the density has been revised to:"];
  printCFDI[densityList];

  (* Clear all local variables not being returned.                      *)
  Clear[noOfDepVars, n1, n2, zeroResult, varDer, posZeroVarDer, nonZeroVarDer,
      constantList, b, newVarDer, strippedVarDer, coefSystem, coefSolution,
      coefSoluprintSRtion, dependentCoefs, rul, partialVarDer, currentTerm,
      termsFormingADerivative, i1, i2, i3, indepVar, printCFDI, lthIVL,
      termBeingMoved];

  Return[{Part[densityList, 1], newFlux}]
] (* end Module checkForDivergences *)

(* #################################################################### *)
(*                                                                      *)
(* ##########       Start of Independence Algorithm          ########## *)
(*                                                                      *)
(* #################################################################### *)

(* ##########            Function: indepDriver             ########## *)

(**********************************************************************)
(* indepDriver[listOfDensities, listOfCompatibilities,                *)
(*     independentVarList, numberDepVars, parameters, eqn, nameEqn]   *)
(* Purpose: A shortened version of analyzeDensitiesForIndependence    *)
(*          which runs the independence algorithm inside the          *)
(*          conservation laws code.                                   *)
(* Input:   A list of densities paired with their compatibility       *)
(*              conditions                                            *)
(*          A list of independent variables                           *)
(*          The number of dependent variables                         *)
(*          A list of constant parameters in the PDE                  *)
(* Output:  A list of independent densities                           *)
(* Created: 30 January 2008 by DP at CSM                              *)
(* Code is in File:  conservationlaws/inanlind.m                      *)
(* Last Modified:  13 November, 2008, 16:54 by DP at CSM              *)
(**********************************************************************)

indepDriver[listOfDensities_, independentVarList_List,
     numberDepVars_, listOfParameters_] :=
Module[{varDerList, combinedDensityList, app, sortedList, currentDenUnderEval,
    workingRhoList, indepDensityList, multipleCk, complete, denLinearCombCk,
    revisedDensity, denEquivalenceCk, printID},

  If[debugIndepDriver, printID = Print, Clear[printID], Clear[printID]];
  printID["debug code: ID, Function: newIndepDriver, File: innewdri.m"];

  printID["Starting the driver function, newIndepDriver."];
  printID["At ID IN, the list of densities given to newIndepDriver:"];
  printID[listOfDensities];
  printID["At ID IN, the list of independent variable contains: ",
      independentVarList];
  printID["At ID IN, there ", If[numberDepVars === 1, "is ", "are "]<>
      "", numberDepVars, " dependent variable"<>
      If[numberDepVars === 1, ".", "s."]];
  printID["At ID IN, the list of parameters contains ", listOfParameters];

  (* Find the variational derivatives for each density.                 *)
  varDerList = Map[Table[variationalDerivativeMultiD[
      #, i1, independentVarList], {i1, 1, numberDepVars}] &,
      Map[Part[#, 1]&, listOfDensities]];
  printID["At ID 1, a list of variational derivatives corresponding to"<>
      " each density in the list respectively:"];
  printID[varDerList];

  (* Combine denisites and variational derivatives into a single list. *)
  combinedDensityList = Thread[app[listOfDensities, varDerList]] /.
      app -> Append;
  printID[ "At ID 2, all densities are placed in the group\n"<>
      " {density, compatibility condition, variational derivative}:"];
  printID[combinedDensityList];

  (* Sort densities so that densities without explicit independent      *)
  (* variables are first in the list and so that densities are ranked   *)
  (* in increasing order of complexity. *)
  sortedList = sortDensities[combinedDensityList];
  printID["At ID 3, the sorted list of densities (all densities without" <>
      " explicit dependence on the independent variables are listed first):"];
  printID[sortedList];

  (* Assume that the first density in the list is independent.          *)
  currentDenUnderEval = Part[sortedList, 1];
  printID["At ID 4, the current density under evaluation:"];
  printID[currentDenUnderEval];

  workingRhoList = Delete[sortedList, 1];
  printID["At ID 5, the list of unevaluated densities has changed to:"];
  printID[workingRhoList];

  (* Place the current density under evaluation into the independent    *)
  (* density list.                                                      *)
  indepDensityList = {currentDenUnderEval};
  printID["At ID 6, the independent density list now contains:"];
  printID[indepDensityList];

  (* Start the comparison of densities.  Work through the list of       *)
  (*  densities one at a time.                                          *)
  currentDenUnderEval = If[workingRhoList =!= {},
      First[workingRhoList], Clear[currentDenUnderEval]];
  While[workingRhoList =!= {},
    printID["At ID 7, the current density under evaluation with its"<>
        " compatibility conditions and variational derivative:"];
    printID[currentDenUnderEval];
    printID["At ID 8, the independent density list now contains:"];
    printID[indepDensityList];

    (* Check the density under evaluation to see if it is a multiple    *)
    (* of an established independent density.                           *)
    multipleCk = checkMultipleDensity[Part[currentDenUnderEval, 1],
        Map[Part[#, 1] &, indepDensityList], independentVarList];
    printID["At ID 9, checkMultipleDensity returns:"];
    printID[multipleCk];

    If[Flatten[multipleCk] === {},
      (* Next, check the density under evaluation to see if it is a     *)
      (* linear combination of known independent densities.             *)
      {complete, denLinearCombCk} = checkForDependency[
          Part[currentDenUnderEval, 1],
          Map[Part[#, 1] &, indepDensityList], {}, den];
      printID["At ID 10, while doing a direct check for linear"<>
          " dependence, checkForDependency finds "<>complete<>" direct"<>
          " linear dependence and returns:"];
      printID[denLinearCombCk];

      If[complete === "revise",
        workingRhoList = Append[workingRhoList,
            Part[indepDensityList, Part[denLinearCombCk, 1]]];
        printID["At ID 11, a density in the independent density list has"<>
            " been found to be possibly dependent.  It is replaced in the"<>
            " evaluation list at the end."];
        printID[workingRhoList];
        indepDensityList = Delete[indepDensityList, {Part[denLinearCombCk, 1]}];
        printID["At ID 12, the suspected independent density is removed"<>
            " from the independent density list."];
        printID[indepDensityList];
        complete = "no"
      ];

      If[complete === "partial",
        revisedDensity = {Part[currentDenUnderEval, 1] -
            Expand[Apply[Plus, Apply[Times, denLinearCombCk, {1}]]],
            Part[revisedDensity, 2], 0};
        Part[revisedDensity, 3] = Table[variationalDerivativeMultiD[
            Part[revisedDensity, 1], i1, independentVarList],
            {i1, 1, numberDepVars}];
        printID["At ID 13, the revised density with compatibility"<>
            " conditions and variational derivative:"];
        printID[revisedDensity],
      (* else *)
        revisedDensity = currentDenUnderEval
      ]; (* end If complete === "partial" *)

      (* Next, check the density under evaluation to see if it is       *)
      (* in an existing equivalence class established by the            *)
      (* densities in the independent density list.                     *)
      (* The variational derivative is used to check equivalence.       *)
      If[complete === "partial" || complete === "no",
        {complete, denEquivalenceCk} = checkForDependency[
            Part[revisedDensity, 3], Map[Part[#, 3] &,
            indepDensityList], listOfParameters, var];
        printID["At ID 14, while doing a check for equivalence"<>
            " checkForDependency finds "<>complete<>" equivalence"<>
            " and returns:"];
        printID[denEquivalenceCk];

        If[complete === "revise",
          workingRhoList = Append[workingRhoList,
              Part[indepDensityList, Part[denLinearCombCk, 1]]];
          printID["At ID 15, a density in the independent density list has"<>
              " been found to be possibly dependent.  It is replaced in the"<>
              " evaluation list at the end."];
          printID[workingRhoList];
          indepDensityList = Delete[indepDensityList, {Part[denLinearCombCk, 1]}];
          printID["At ID 16, the suspected independent density is removed"<>
              " from the independent density list."];
          printID[indepDensityList]
        ];

        If[complete === "partial",
          revisedDensity = {Part[revisedDensity, 1] -
              Expand[Apply[Plus, Apply[Times, denLinearCombCk, {1}]]],
              Part[revisedDensity, 2], 0};
          printID["At ID 17, the revised density with compatibility"<>
              " conditions and variational derivative:"];
          printID[revisedDensity];
        ]; (* end If complete === "partial" *)

        If[complete === "full", revisedDensity = {}];

        If[revisedDensity =!= {},
          indepDensityList = Append[indepDensityList, revisedDensity]];
        printID["At ID 18, the independent density list now contains:"];
        printID[indepDensityList]
      ] (* end If complete === "partial" || complete === "no" *)
    ]; (* end If Flatten[multipleCk] *)

    (* Reset by assigning the next density up for evaluation.           *)
    workingRhoList = Drop[workingRhoList, 1];
    If[workingRhoList =!= {},
      currentDenUnderEval = First[workingRhoList]
    ] (* end If workingRhoList *)
  ]; (* end While *)

  indepDensityList = Map[Drop[#, -1] &, indepDensityList];

  printID["At ID OUT, the independent density list contains:"];
  printID[indepDensityList];

  (* Clear all local variables not being returned.                      *)
  Clear[varDerList, combinedDensityList, app, sortedList, currentDenUnderEval,
      workingRhoList, multipleCk, complete, denLinearCombCk, revisedDensity,
      denEquivalenceCk, printID];

  Return[indepDensityList]
] (* end Module analyzeDensitiesForIndependence *)

(* ##########  Function: analyzeDensitiesForIndependence   ########## *)

(**********************************************************************)
(* analyzeDensitiesForIndependence[listOfDensities, independVarList,  *)
(*     numberDepVars, parameters, eqn, nameEqn]                       *)
(* Purpose: A driver function which runs all functions associated     *)
(*          checking a list of densities for independence.            *)
(* Input:   A list of densities paired with their compatibility       *)
(*              conditions                                            *)
(*          A list of independent variables                           *)
(*          The number of dependent variables                         *)
(*          A list of constant parameters in the PDE                  *)
(*          The system of PDEs for which the density is being         *)
(*              evaluated                                             *)
(*          The name of the PDE                                       *)
(* Output:  A list of independent densities                           *)
(* Created: 30 January 2008 by DP at CSM                              *)
(* Code is in File:  conservationlaws/inanlind.m                      *)
(* Last Modified:  13 November, 2008, 16:52 by DP at CSM              *)
(**********************************************************************)

analyzeDensitiesForIndependence[listOfDensities_, independentVarList_List,
     numberDepVars_, listOfParameters_, eqn_List, nameEqn_] :=
Module[{varDerList, combinedDensityList, sortedList, currentDenUnderEval,
    termByTermEval = {1}, ans, workingRhoList, j1 = 1, indepDensityList,
    multipleCk, denLinearCombCk, denEquivalenceCk, dword, dword2,
    origDenNo, complete, revisedDensity, positionIndDen, indepDens,
    independentDensity, lthivl, workingDensity, assocFlux, evolutionRules,
    rank, check, printcom, printADI},

  If[debugAnalyzeDensities, printADI = Print, Clear[printADI],
      Clear[printADI]];
  printADI["debug code: ADI, Function: analyzeDensitiesForIndependence,"<>
      " File: inanlind.m"];

  printADI["Starting the driver function, newIndepDriver."];
  printADI["At ADI IN, the list of densities given to newIndepDriver:"];
  printADI[listOfDensities];
  printADI["At ADI IN, the list of independent variable contains: ",
      independentVarList];
  printADI["At ADI IN, there ", If[numberDepVars === 1, "is ", "are "]<>
      "", numberDepVars, " dependent variable"<>
      If[numberDepVars === 1, ".", "s."]];
  printADI["At ADI IN, the list of parameters contains ", listOfParameters];

  (* Initializations                                                    *)
  Clear[independentDensityList];
  lthivl = Length[independentVarList];
  dword = If[lthivl === 1, "total derivative", "divergence"];
  If[Flatten[Map[Part[#, 2] &, listOfDensities]] === {},
    Clear[printcom], printcom = Print];

  (* Print opening statements.                                          *)
  dword2 = Sequence[StyleForm[If[lthivl === 1, Subscript["D","x"], "Div"],
      If[lthivl === 1, FontSlant -> Italic, FontSlant -> Plain]],
      StyleForm["J", If[lthivl === 1, FontSlant -> Italic,
      FontWeight -> Bold]]];
  Print["-------------------------------------------------------"];
  Print["BEGINNING THE ANALYSIS OF DENSITIES ON CONSERVATION LAWS"<>
      ""<>If[eqn =!= {}, " FOR THE "], If[eqn =!= {}, nameEqn]];
  If[eqn =!= {}, Map[Print[pdeform[#], " == 0"] &, eqn]];
  Print["\n\nA conservation law is"];
  Print["       ", StyleForm[Subscript["D", "t"], FontSlant -> Italic],
      \[Rho], " + ", dword2, " = 0,                  (1)"];
  Print["where ", \[Rho], " is the density and ", StyleForm["J",
      If[lthivl === 1, FontSlant -> Italic, FontWeight -> Bold]],
      " is the associated flux."];

  (* Find the variational derivatives for each density.                 *)
  varDerList = Map[Table[variationalDerivativeMultiD[
      #, i1, independentVarList], {i1, 1, numberDepVars}] &,
      Map[Part[#, 1]&, listOfDensities]];
  printADI["At ADI 1, a list of variational derivatives corresponding to"<>
      " each density in the list respectively:"];
  printADI[varDerList];

  (* Combine denisites and variational derivatives into a single list. *)
  combinedDensityList = Map[{Part[#, 1], Part[#, 2]} &, listOfDensities];
  combinedDensityList = Thread[app[combinedDensityList, varDerList]] /.
      app -> Append;
  printADI[ "At ADI 2, all densities are placed in the group\n"<>
      " {density, compatibility condition, variational derivative}:"];
  printADI[combinedDensityList];

  (* Check densities for total derivatives.  Any densities that are     *)
  (* total derivatives will be removed from the list and will be        *)
  (* reported.                                                          *)
  sortedList = totalDivergenceCheck[combinedDensityList, independentVarList,
      numberDepVars];
  printADI["At ADI 3, all densities that are total divergences have been"<>
      " removed from the list of densities:"];
  printADI[sortedList];

  If[sortedList === {},
    Print["All densities have been found to form trivial conservaton laws."];
    Return[]];

  (* Sort densities so that densities without explicit independent      *)
  (* variables are first in the list and so that densities are ranked   *)
  (* in increasing order of complexity. *)
  sortedList = sortDensities[sortedList];
  printADI["At ADI 5, the sorted list of densities (all densities without" <>
      " explicit dependence on the independent variables are listed first):"];
  printADI[sortedList];
  Print["-------------------------------------------------------"];
  Print["The densities given to the program have been reorganized by"<>
      " the number of terms and the complexity of the terms.  Densities"<>
      " with the fewest number of terms and lowest order are tested first"<>
      " followed by densities of increasing complexity.  Densities" <>
      " without explicit independent variables will be checked first." <>
      "  Densities with compatibility conditions will be checked last."];

  (* Assume that the first density in the list is independent.          *)
  While[Flatten[termByTermEval] =!= {},
    currentDenUnderEval = Part[sortedList, j1];
    printADI["At ADI 6, the current density under evaluation:"];
    printADI[currentDenUnderEval];

    termByTermEval = checkDensityForEquivs[Part[currentDenUnderEval, 1],
        independentVarList, numberDepVars, listOfParameters];
    printADI["At ADI 7, checkDensityForEquivs returns:"];
    printADI[termByTermEval];

    If[j1 > Length[sortedList],
      Print["All densities provided to the program contain terms that are"<>
          " either total divergences or divergence-equivalent to another"<>
          " term. It is recommended that all densities be rewritten with"<>
          " these terms shifted into the flux before continuing."];
      Print["All computations are being discontinued."];
      Abort[]
    ]; (* end If j1 *)
    j1++
  ]; (* end While *)

  workingRhoList = Delete[sortedList, j1 - 1];
  printADI["At ADI 8, the list of unevaluated densities has changed to:"];
  printADI[workingRhoList];

  (* Print a report stating information about the first density deemed  *)
  (* to be independent.                                                 *)
  Print["-------------------------------------------------------"];
  Print["The density with the simplest form as determined by the program,"];
  Print["    ", pdeform[Part[currentDenUnderEval, 1]]];
  If[Part[currentDenUnderEval, 2] === {},
    printcom["    with no conditions on any parameter,"],
    printcom["    with the condition(s) ", Part[currentDenUnderEval, 2],
        " on the parameters."]
  ];(*end If Part*)
  Print["  is automatically considered independent.  It is placed into"<>
      " the list of independent densities."];

  (* Place the current density under evaluation into the independent    *)
  (* density list.                                                      *)
  indepDensityList = {currentDenUnderEval};
  printADI["At ADI 9, the independent density list now contains:"];
  printADI[indepDensityList];

  (* Start the comparison of densities.  Work through the list of       *)
  (*  densities one at a time.                                          *)
  currentDenUnderEval = If[workingRhoList =!= {},
      First[workingRhoList], Clear[currentDenUnderEval]];
  While[workingRhoList =!= {},
    printADI["At ADI 10, the current density under evaluation with its"<>
        " compatibility conditions and variational derivative:"];
    printADI[currentDenUnderEval];
    printADI["At ADI 11, the independent density list now contains:"];
    printADI[indepDensityList];

    origDenNo = Cases[Position[Expand[listOfDensities],
        Expand[Part[currentDenUnderEval, 1]]], n1_ /; Length[n1] === 2];
    origDenNo = Part[Flatten[origDenNo], 1];
    printADI["At ADI 12, the density currently being evaluated was in"<>
        " position ", origDenNo, " in the original density list."];

    Print["-------------------------------------------------------"];
    Print["DENSITY NOW BEING EVALUATED:"];
    Print["  Density no. ", origDenNo, " from the original list"<>
        " of densities:"];
    Print["    ", pdeform[Part[currentDenUnderEval, 1]]];
    If[Part[currentDenUnderEval, 2] === {},
      printcom["    with no conditions on any of the parameters."],
      printcom["    with the condition(s) on the parameters,"<>
          "", Part[currentDenUnderEval, 2], " applied to the denisty."]
    ]; (* end If Part[currentDenUnderEval, 2] *)
    (* Check the density being evaluated for terms that are             *)
    (* divergences and/or divergence-equivalent.                        *)
    termByTermEval = checkDensityForEquivs[Part[currentDenUnderEval, 1],
        independentVarList, numberDepVars, listOfParameters];
    printADI["At ADI 13, checkDensityForEquivs returns:"];
    printADI[termByTermEval];

    (* Print out the results from the term-by-term evaluation.          *)
    If[termByTermEval =!= {{},{}},
      reportDensityEvaluation[termByTermEval, independentVarList];

      (* Revise the given density by removing all terms found in the    *)
      (* evaluation.                                                    *)
      revisedDensity = {Expand[Part[currentDenUnderEval, 1] -
          Apply[Plus, Flatten[termByTermEval]]],
          Part[currentDenUnderEval, 2], 0};
      Part[revisedDensity, 3] = Table[variationalDerivativeMultiD[
          Part[revisedDensity, 1], i1, independentVarList],
          {i1, 1, numberDepVars}];
      printADI["At ADI 14, the revised density is not:"];
      printADI[revisedDensity];

      Print["* All "<>dword<>" terms and all "<>dword<>"-equivalent"<>
          "\n  terms are removed.  Density no. ", origDenNo, ""<>
          " now consists of"];
      Print["  ", pdeform[Part[revisedDensity, 1]]];
      Print["  The evaluation of Density no. ", origDenNo, " continues."],

    (* else *)
      revisedDensity = currentDenUnderEval
    ]; (* end If termByTermEval *)

    (* Check the density under evaluation to see if it is a multiple    *)
    (* of an established independent density.                           *)
    multipleCk = checkMultipleDensity[Part[revisedDensity, 1],
        Map[Part[#, 1] &, indepDensityList], independentVarList];
    printADI["At ADI 15, checkMultipleDensity returns:"];
    printADI[multipleCk];

    If[Flatten[multipleCk] =!= {},
      Print["A check to see if the given density is a direct multiple of"<>
          " another density yields the following information:"];
      Print["* Density no. ", origDenNo, " is the same as established"<>
          " independent density"];
      Print["  ", pdeform[Part[multipleCk, 1] /. List -> Sequence]];
      Print["  multiplied by the factor ", Part[multipleCk, 2, 1], "."];
      Print["  The evaluation of Density no. ", origDenNo, " is complete."<>
            "  It has no independent part."],

    (* else the density is NOT a multiple *)
      (* Next, check the density under evaluation to see if it is a     *)
      (* linear combination of known independent densities.             *)
      {complete, denLinearCombCk} = checkForDependency[Part[revisedDensity, 1],
          Map[Part[#, 1] &, indepDensityList], {}, den];
      printADI["At ADI 16, while doing a direct check for linear"<>
          " dependence, checkForDependency finds "<>complete<>" direct"<>
          " linear dependence and returns:"];
      printADI[denLinearCombCk];

      If[complete === "revise",
        workingRhoList = Append[workingRhoList,
            Part[indepDensityList, Part[denLinearCombCk, 1]]];
        printADI["At ADI 17, a density in the independent density list has"<>
            " been found to be possibly dependent.  It is replaced in the"<>
            " evaluation list at the end."];
        printADI[workingRhoList];
        indepDensityList = Delete[indepDensityList, {Part[denLinearCombCk, 1]}];
        printID["At ADI 18, the suspected independent density is removed"<>
            " from the independent density list."];
        printID[indepDensityList];
        denLinearCombCk = {"no", {}}
        ]; (* end If complete *)

      If[complete === "full",
        Print["A check for direct linear dependence on other densities"<>
            " yields the following information."];
        Print["* Density no. ", origDenNo, " is formed by making the"<>
            " following linear\n  combination of established independent"<>
            " densities:"];
        Map[Print["  ", pdeform[Part[#, 2]],
            " multiplied by ", Part[#, 1]] &, denLinearCombCk];
        Print["The evaluation of Density no. ", origDenNo, " is complete."<>
            "  It has no independent part."],

      (* else if not "full" *)
        If[complete === "partial",
          revisedDensity = {Part[revisedDensity, 1] -
              Expand[Apply[Plus, Apply[Times, denLinearCombCk, {1}]]],
              Part[revisedDensity, 2], 0};
          Part[revisedDensity, 3] = Table[variationalDerivativeMultiD[
              Part[revisedDensity, 1], i1, independentVarList],
              {i1, 1, numberDepVars}];
          printADI["At ADI 19, the revised density with compatibility"<>
              " conditions and variational derivative:"];
          printADI[revisedDensity];

          Print["A check for direct linear dependence on other densities"<>
              " yields the following information."];
          If[Length[denLinearCombCk] === 1,
            Print["* Density no. ", origDenNo, " is partially dependent. "<>
                " The density contains the established independent density"];
            Print["  ", pdeform[Part[denLinearCombCk, 1, 2]], " multiplied by ",
                Part[denLinearCombCk, 1, 1], "."],
          (* else *)
            Print["* Density no. ", origDenNo, " is partially dependent. "<>
                " The density contains a linear combination of the"<>
                " established independent densities"];
            Map[Print["  The density " , pdeform[Part[#, 2]],
                " multiplied by ", Part[#, 1]] &, denLinearCombCk]
          ]; (* end If denLinearCombCk *)

          Print["* All linearly dependent terms are removed.  Density no. ",
              origDenNo, " now consists of"];
          Print["  ", pdeform[Part[revisedDensity, 1]]];
          Print["  The evaluation of Density no. ", origDenNo, " continues."];
        ]; (* end If complete === "partial" *)

        (* Next, check the density under evaluation to see if it is     *)
        (* in an existing equivalence class established by the          *)
        (* densities in the independent density list.                   *)
        (* The variational derivative is used to check equivalence.     *)
        {complete, denEquivalenceCk} = checkForDependency[
            Part[revisedDensity, 3], Map[Part[#, 3] &,
            indepDensityList], listOfParameters, var];

        printADI["At ADI 20, while doing a check for equivalence"<>
            " checkForDependency finds "<>complete<>" equivalence"<>
            " and returns:"];
        printADI[denEquivalenceCk];

        If[complete === "revise",
          workingRhoList = Append[workingRhoList,
              Part[indepDensityList, Part[denLinearCombCk, 1]]];
          printADI["At ADI 21, a density in the independent density list has"<>
              " been found to be possibly dependent.  It is replaced in the"<>
              " evaluation list at the end."];
          printID[workingRhoList];
          indepDensityList = Delete[indepDensityList, {Part[denLinearCombCk, 1]}];
          printADI["At ADI 22, the suspected independent density is removed"<>
              " from the independent density list."];
          printID[indepDensityList];
          denEquivalenceCk = {"no", {}}
        ]; (* end If complete *)

        independentDensity = reportEquivalence[complete, origDenNo,
            currentDenUnderEval, revisedDensity, indepDensityList,
            denEquivalenceCk, independentVarList, numberDepVars,
            listOfParameters];
        printADI["At ADI 23, the independent density returned from the"<>
            " equivalence report:"];
        printADI[independentDensity];

        If[independentDensity =!= {},
          indepDensityList = Append[indepDensityList, independentDensity]];
      ]; (* end If Flatten[multipleCk] *)
    ]; (* end If Part[currentDenUnderEval, 3] *)

    (* Reset by assigning the next density up for evaluation.           *)
    workingRhoList = Drop[workingRhoList, 1];
    If[workingRhoList =!= {},
      currentDenUnderEval = First[workingRhoList]
    ] (* end If workingRhoList *)
  ]; (* end While *)

  (* Make the list of independent conservation laws available to the    *)
  (* user. Assign independent list to a global variable so that the     *)
  (* user can have access to this information.                          *)
  listOfIndCL = {};
  While[indepDensityList =!= {},
    workingDensity = First[indepDensityList];
    printADI["At ADI 20, the current density (with compatibility"<>
        " conditions and variational derivative to be matched to a flux:"];
    printADI[workingDensity];

    workingDensity = Drop[workingDensity, -1];
    printADI["At ADI 21, the variational derivative is removed from the"<>
        "list of density information:"];
    printADI[workingDensity];

    (* Attach the associated flux to the density information.           *)
    assocFlux = Cases[Position[listOfDensities,
        Part[workingDensity, 1]], {_, _}];
    assocFlux = If[assocFlux =!= {}, First[assocFlux], assocFlux];
    printADI["At ADI 22, the position of the density in the original"<>
        " list of information given to the function:"];
    printADI[assocFlux];

    rank = assocFlux /. {n1_, 1} -> {n1, 3};
    rank = Extract[listOfDensities, rank];
    printADI["At ADI 23, the rank of the density:"];
      printADI[rank];

    If[assocFlux === {},
      printADI["At ADI 24, there is a density for which no flux exists"<>
          " in the original density list."];

      evolutionRules = checkForEvolutionForm[eqn, independentVarList];
      printADI["At ADI 25, the original PDE is in evolution form: ",
          evolutionRules];

      If[evolutionRules === "Yes",
        evolutionRules = makeEvolutionRules[eqn];
        assocFlux = Flatten[matchDensityWithFlux[{workingDensity},
            evolutionRules, numberDepVars, independentVarList,
            listOfParameters, {}]];
        assocFlux = First[assocFlux],

      (* else *)
        assocFlux = "The corresponding flux for this density cannot be"<>
            " determined since the PDE given is not in evolution form."<>
            "  Rerun condenMD with formRho set equal to this denisty in"<>
            " the data file to find the corresponding flux."
      ]; (* end If evolutionRules *)
      printADI["At ADI 26, the recalculated flux is:"];
      printADI[assocFlux],

    (* else *)
      assocFlux = assocFlux /. {n1_, 1} -> {n1, 4};
      printADI["At ADI 27, the position of the corresponding flux in the"<>
          " original list of information given to the function:"];
      printADI[assocFlux];

      assocFlux = Extract[listOfDensities, assocFlux];
      printADI["At ADI 28, the corresponding flux:"];
      printADI[assocFlux]
    ]; (* end If assocFlux *)

    (* Recheck the validity of the conservation law.                    *)
    check = If[Not[StringQ[assocFlux]] && assocFlux =!= {},
        explicitVerification[eqn, Part[workingDensity, 1],
        Part[workingDensity, 2], assocFlux, independentVarList],
        "Cannot be determined"];
    printADI["At ADI 29, explicit verification of the density-flux pair:"];
    printADI[check];

    workingDensity = Append[workingDensity, assocFlux];
    workingDensity = Append[workingDensity, rank];
    workingDensity = Append[workingDensity, check];
    printADI["At ADI 30, combined density-flux information for the current"<>
        " density:"];
    printADI[workingDensity];

    listOfIndCL = Append[listOfIndCL, workingDensity];
    printADI["At ADI 31, list of all combined density-flux information:"];
    printADI[listOfIndCL];

    indepDensityList = If[indepDensityList =!= {},
        Drop[indepDensityList, 1], {}];
  ]; (* end While *)

  printADI["At ADI OUT, the independent density list now contains:"];
  printADI[indepDensityList];

  (* Clear all local variables not being returned.                      *)
  Clear[varDerList, combinedDensityList, sortedList, currentDenUnderEval,
      termByTermEval, ans, workingRhoList, j1, indepDensityList,
      multipleCk, denLinearCombCk, denEquivalenceCk, dword, dword2,
      origDenNo, complete, revisedDensity, positionIndDen, indepDens,
      independentDensity, lthivl, workingDensity, assocFlux, evolutionRules,
      rank, check, printcom, printADI];

] (* end Module analyzeDensitiesForIndependence *)

(* ##########           Function: sortDensities            ########## *)

(**********************************************************************)
(* sortDensities[densityList]                                         *)
(* Purpose: To organize a list of given desities so that all          *)
(*          densities without explicit dependence on the independent  *)
(*          variables are placed ahead of densities with explicit     *)
(*          dependence, and so that densities are listed in increasing*)
(*          order of complexity.                                      *)
(* Input:   A list of densities                                       *)
(* Output:  A list of densities with a revised order                  *)
(* Created: 2 May 2007 by DP at CSM                                   *)
(* Code is in File:  independence/insortde.m                          *)
(* Last Modified:  18 March, 2008, 18:04 by DP at CSM                 *)
(**********************************************************************)

sortDensities[densityList_List] := Module[{workingList, numOfTerms,
    densityFactors, degreeOfUTerms, degreeOfDerTerms, degreeOfTerms,
    termSimplicity, positionOfExplicit, explicitList, explicitWOCompat,
    nonexplicitList, nonexplicitWOCompat, combinedDensityList, n1, n2,
    dumlist, printSD},

  If[debugSortDensities, printSD = Print, Clear[printSD], Clear[printSD]];
  printSD["debug code: SD, Function: sortDensities, File: insortde.m"];

  printSD["At SD IN,  the list of densities:"];
  printSD[densityList];

  (* Initialization *)
  expandedDensityList = Expand[densityList];

  (* Sort densities according to whether or not they contain any        *)
  (* explicit independent variables, number of terms, overall degree    *)
  (* and simplicity of terms.                                           *)
  workingList = Map[List, Map[Part[#, 1] &, expandedDensityList]] /.
      Plus -> Sequence;
  printSD["At SD 1, a list of the densities with terms listed instead of"<>
      " added:"];
  printSD[workingList];

  numOfTerms = Map[Length, workingList];
  printSD["At SD 2, a list of the number of terms in each density:"];
  printSD[numOfTerms];

  densityFactors = Map[FactorList, Expand[workingList],{2}];
  printSD["At SD 3, all denisties have been completely factored:"];
  printSD[densityFactors];

  (* Create a ranking system.  degreeOfTerms is sum of the degrees on   *)
  (* all terms for each density.  simplicity shows whether the          *)
  (* highest degree term has a single u or is form from a product of    *)
  (* u's and derivatives of u's.                                        *)
  degreeOfUTerms = Map[Cases[#, List[u[_][__], _], {0, Infinity}] &,
       densityFactors];
  printSD["At SD 4, a list of the degrees of terms containing u[_] for"<>
      " each density:"];
  printSD[degreeOfUTerms];

  degreeOfDerTerms = Map[Cases[#, List[Derivative[__][u[_]][__], _],
      {0, Infinity}] &, densityFactors];
  printSD["At SD 5, a list of the degrees of terms containing derivatives"<>
      " of u[_] for each density:"];
  printSD[degreeOfDerTerms];

  degreeOfTerms = Thread[List[Map[Part[#, 2] &, degreeOfUTerms, {2}],
      Map[Part[#, 2] &, degreeOfDerTerms, {2}]]];
  printSD["At SD 6, a complete list of the degrees of all terms for each"<>
      " density:"];
  printSD[degreeOfTerms];

  termSimplicity = Apply[Max, degreeOfTerms, {1, 2}];
  degreeOfTerms = Apply[Plus, Apply[Sequence, degreeOfTerms, {2}], {1}];
  termSimplicity = degreeOfTerms - termSimplicity;
  printSD["At SD 7, a list of the degrees of terms in each density:"];
  printSD[degreeOfTerms];
  printSD["At SD 8, a list of values representing term simplicity:"];
  printSD[termSimplicity];

  positionOfExplicit = Position[densityFactors,
      List[n1_,_]/; n1 === x || n1 === y || n1 === z || n1 === t];
  positionOfExplicit = Union[Map[Part[#, 1] &, positionOfExplicit]];
  printSD["At SD 9, the positions where explicit variables occur in"<>
      " density terms are:"];
  printSD[positionOfExplicit];

  (* Connect each density with it's number of terms, degree of terms.   *)
  workingList = Thread[List[degreeOfTerms, numOfTerms,
      termSimplicity, expandedDensityList]];
  workingList = Apply[Sequence, workingList, {2}];
  printSD["At SD 10, The list of densities paired with information about"<>
      " number of terms and degree of terms:"];
  printSD[workingList];

  (* Make a list of densities containing explicit independent variables.*)
  explicitList = Extract[workingList, Transpose[{positionOfExplicit}]];
  printSD["At SD 11, a list of densities containing explicit independent"<>
      " variables:"];
  printSD[explicitList];

  (* Make a list of densities without explicit independent variables.   *)
  nonexplicitList = Complement[workingList, explicitList];
  printSD["At SD 12, a list of densities without explicit independent"<>
      " variables:"];
  printSD[nonexplicitList];

  (* Reorganize the list of densities so that                           *)
  (*    densities without explicit independent variables are first      *)
  (*    densities without compatibility conditions are first            *)
  (*    densities are ordered by degree                                 *)
  (*    densities are ordered by number of terms if they have the same  *)
  (*         degree                                                     *)
  explicitWOCompat = Cases[explicitList, List[_, _, _, _, n1_, _] /; n1 === {},
      {0, Infinity}];
  explicitWOCompat = Sort[explicitWOCompat];
  printSD["At SD 13, a list of densities containing explicit independent"<>
      " variables and no compatibility conditions:"];
  printSD[explicitWOCompat];

  explicitWithCompat = Complement[explicitList, explicitWOCompat];
  explicitWithCompat = Sort[explicitWithCompat];
  printSD["At SD 14, a list of densities containing explicit independent"<>
      " variables with compatibility conditions:"];
  printSD[explicitWithCompat];

  nonexplicitWOCompat = Cases[nonexplicitList,
      List[_, _, _, _, n1_, _] /; n1 === {}, {0, Infinity}];
  nonexplicitWOCompat = Sort[nonexplicitWOCompat];
  printSD["At SD 15, a list of densities without explicit independent"<>
      " variables and no compatibility conditions:"];
  printSD[nonexplicitWOCompat];

  nonexplicitWithCompat = Complement[nonexplicitList, nonexplicitWOCompat];
  nonexplicitWithCompat = Sort[nonexplicitWithCompat];
  printSD["At SD 16, a list of densities without explicit independent"<>
      " variables, but with compatibility conditions:"];
  printSD[nonexplicitWithCompat];

  combinedDensityList = Flatten[{nonexplicitWOCompat, explicitWOCompat,
      nonexplicitWithCompat, explicitWithCompat}, 1];
  combinedDensityList = Apply[dumlist, combinedDensityList] /.
      List[_, _, _, n1_, n2_, n3_] -> List[n1, n2, n3];
  combinedDensityList = combinedDensityList /. dumlist -> List;
  printSD["At SD 17, all denisties after sorting is finished:"];
  printSD[combinedDensityList];

  Clear[workingList, numOfTerms, densityFactors, degreeOfUTerms,
      degreeOfDerTerms, degreeOfTerms, termSimplicity, positionOfExplicit,
      explicitList, explicitWOCompat, nonexplicitList, nonexplicitWOCompat,
      n1, n2, dumlist, printSD];

Return[combinedDensityList]

] (* end Module sortDensities *)

(* ##########       Function: totalDivergenceCheck         ########## *)

(**********************************************************************)
(* totalDivergenceCheck[densityList, indepVarList, numDepVars]        *)
(* Purpose: To check a list of calculated densities for any densities *)
(*          that might be total derivatives.                          *)
(* Input:   A list of densities                                       *)
(*          A list of corresponding variational derivatives           *)
(*          A list of independent variables                           *)
(*          The number of dependent variables.                        *)
(* Output:  A revised list of densities with the total divergences    *)
(*          removed.                                                  *)
(* Created: 3 May 2007 by DP at CSM                                   *)
(* Code is in File:  independence/intodeck.m                          *)
(* Last Modified:  21 February, 2008, 11:58 by DP at CSM              *)
(**********************************************************************)

totalDivergenceCheck[densityList_List, indepVars_List, numDepVars_] :=
Module[{zeroPosition, lz, densitiesToBeRemoved,
    lengthIndepVars = Length[indepVars], printTDC},

  If[debugTotalDivergenceCheck, printTDC = Print, Clear[printTDC],
      Clear[printTDC]];
  printTDC["debug code: TDC, Function: totalDivergenceCheck,
      File: intodeck.m"];

  (* Find the positions of any 0 variational derivatives.               *)
  zeroPosition = Position[densityList, Table[0, {i1, 1, numDepVars}]];
  zeroPosition = Map[Take[#, 1] &, zeroPosition];
  printTDC["At TDC 1, zero variational derivatives are at position(s):"];
  printTDC[zeroPosition];

  (* Report any total divergences so the the user knows that they have  *)
  (* been taken from the list                                           *)
  If[zeroPosition =!= {},
    lz = Length[zeroPosition];
    Print["-------------------------------------------------------"];
    Print["There " <> If[lz === 1, "is ", "are "] <> ToString[lz] <>
        " densit" <> If[lz === 1, "y in the given list that is a ",
        "ies in the given list that are "] <> "total " <>
        If[lengthIndepVars === 1,
        "derivative" <> If[lz === 1, ".  ", "s.  " ],
        "divergence" <> If[lz === 1, ".  ", "s.  " ]] <>
        If[lz === 1, "This ", "These "] <>
        "will be removed from the list since " <>
        If[lz === 1, "it ", "they "] <>
        "can be incorporated into the flux.  The densit" <>
        If[lz === 1, "y", "ies"] <> " removed " <>
        If[lz === 1, "is:", "are:"]];
    densitiesToBeRemoved = Map[Extract[densityList, #] &, zeroPosition];
    Map[Print["       ", pdeform[Part[#, 1]]] &, densitiesToBeRemoved],

  (* else *)
    densitiesToBeRemoved =  {};
  ]; (* end If zeroPosition *)

  Clear[zeroPosition, lz, lengthIndepVars, printTDC];

  (* Return all remaining densities.                                    *)
  Return[Complement[densityList, densitiesToBeRemoved]];

] (* end Module totalDerivativeCheck *)

(* ##########       Function: checkDensityForEquivs        ########## *)

(**********************************************************************)
(* checkDensityForEquivs[density, indepVarsList, noDepVars,           *)
(*     parameters]                                                    *)
(* Purpose: To check a density for terms that may be a total          *)
(*          divergence or that may be divergence-equivalent.          *)
(* Input:   A density                                                 *)
(*          A list of independent variables                           *)
(*          The number of dependent variables                         *)
(*          A list of parameters in the problem                       *)
(* Output:  A list of terms that are total divergences                *)
(*          A list of terms that are divergence-equivalent            *)
(* Created: 28 January, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inchdefe.m                          *)
(* Last Modified: 18 March, 2008, 16:09 by DP at CSM                  *)
(**********************************************************************)

checkDensityForEquivs[density_, indepVarsList_, noDepVars_, parameters_] :=
Module[{lengthIndList, workingDensity, varDerPerTerm, positionZeroVarDer,
    totalDerivList, sys, checkForEquiv, coefficientsLeft, divEquivList,
    printCDFE},

  If[debugCheckDensityForEquivs, printCDFE = Print, Clear[printCDFE],
      Clear[printCDFE]];
  printCDFE["debug code: CDFE, Function: checkDensityForEquivs,"<>
      " File: inchdefe.m"];

  printCDFE["At CDFE IN, the given density is:"];
  printCDFE[density];
  printCDFE["At CDFE IN, the independent variable list is ", indepVarsList];
  printCDFE["At CDFE IN, there are ", noDepVars, " dependent variables"];
  printCDFE["At CDFE IN, the list of parameters is ", parameters];

  (* Initializations                                                    *)
  Clear[p];
  zero = Table[0, {j1, 1, noDepVars}];
  lengthIndList = Length[indepVarsList];
  workingDensity = Expand[density];
  coefficientList = Table[p[i1],
      {i1, 1, If[Head[density] === Plus, Length[workingDensity], 1]}];

  (* Strip off coefficients from all terms.                             *)
  strippedDensity = stripper[workingDensity];
  printCDFE["At CDFE 1, a list stripped terms found in the given density:"];
  printCDFE[strippedDensity];

  listOfDensityTerms = If[Head[workingDensity] === Plus,
      Apply[List, workingDensity], {workingDensity}];
  listOfDensityTerms = Thread[Times[coefficientList, listOfDensityTerms]];
  printCDFE["At CDFE 2, undetermined coefficients have been multiplied to"<>
      " each term in the density:"];
  printCDFE[listOfDensityTerms];

  (* Find the variational derivative for each term in the density.      *)
  varDerPerTerm = Map[Table[
      variationalDerivativeMultiD[#, j1, indepVarsList],
      {j1, 1, noDepVars}] &, listOfDensityTerms];
  printCDFE["At CDFE 3, a list of variational derivatives for each term"<>
      " in the density:"];
  printCDFE[varDerPerTerm];

  (* Check terms for total divergences.                                 *)
  positionZeroVarDer = Position[varDerPerTerm, zero];
  printCDFE["At CDFE 4, a list of positions where the variational"<>
      " derivative of a term is zero:"];
  printCDFE[positionZeroVarDer];

  totalDerivList = Map[Extract[workingDensity, #] &, positionZeroVarDer];
  printCDFE["At CDFE 5, a list of terms that are total ",
      If[lengthIndList > 1, "divergence", "derivative"], "s:"];
  printCDFE[totalDerivList];

  removeDivs = Apply[p, positionZeroVarDer, {1}] /. n1_p -> Rule[n1, 0];
  printCDFE["At CDFE 6, a list of rules to remove total divergences:"];
  printCDFE[removeDivs];

  (* Check for terms or groups of terms that may be                     *)
  (* divergence-equivalent.                                             *)
  {sys, checkForEquiv} = checkLinearCombInd[varDerPerTerm, p, parameters];
  checkForEquiv = Flatten[checkForEquiv];
  printCDFE["At CDFE 7, after checking the terms for linear independence,"<>
      " these results are returned:"];
  printCDFE[checkForEquiv];

  (* A term that is divergence-equivalent can occur twice from the      *)
  (* divergence applied to two different expressions.  The next steps   *)
  (* separate the parts from which the divergence term is obtained.     *)
  listOfDensityTerms = Expand[listOfDensityTerms /.
      Union[removeDivs, checkForEquiv]];
  printCDFE["At CDFE 8, the undetermined coefficients in the list of"<>
      " density terms have been adjusted by applying the solution obtained"<>
      " from the system created from the variational derivatives:"];
  printCDFE[listOfDensityTerms];

  listOfDensityTerms = Expand[Apply[Plus, listOfDensityTerms]];
  printCDFE["At CDFE 9, the list of terms has been changed into an"<>
      " expression: (Several terms may disappear)"];
  printCDFE[listOfDensityTerms];

  listOfDensityTerms = Apply[List, listOfDensityTerms];
  printCDFE["At CDFE 10, the expression is changed back into a list of terms:"];
  printCDFE[listOfDensityTerms];

  coefficientsLeft = Union[Cases[listOfDensityTerms, _p, {0, Infinity}]];
  printCDFE["At CDFE 11, all undetermined coefficients left in the density"<>
      "list:"];
  printCDFE[coefficientsLeft];

  divEquivList = Map[Cases[listOfDensityTerms, Times[___, #],
      {0, Infinity}] &, coefficientsLeft] /. p[_] -> 1;
  printCDFE["At CDFE 12, lists of equivalent densities:"];
  printCDFE[divEquivList];

  printio["Leaving checkDensityForEquivs with the list of total divergences:"];
  printio[totalDerivList];
  printio["Leaving checkDensityForEquivs with the list of divergence-"<>
      "equivalent terms:"];
  printio[divEquivList];

  Clear[lengthIndList, workingDensity, varDerPerTerm, positionZeroVarDer,  sys,
      checkForEquiv, coefficientsLeft, printCDFE];

  Return[{totalDerivList, divEquivList}]
  ] (* end Module checkDensityForEquivs *)

(* ##########        Function: checkMultipleDensity        ########## *)

(**********************************************************************)
(* checkMultipleDensity[density, indepDensityList, indVarList]        *)
(* Purpose: To check a density for terms that may be a total          *)
(*          divergence or that may be divergence-equivalent.          *)
(* Input:   The density currently being evaluated                     *)
(*          A list of densities known to be independent               *)
(*          A list of independent variables                           *)
(* Output:  If the density currently being evaluated is a multiple of *)
(*          a known independent density, the density it is a multiple *)
(*          of will be given along with the multiplication factor     *)
(* Created: 28 January, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inchmude.m                          *)
(* Last Modified:  13 May, 2008, 17:09 by DP at CSM                   *)
(**********************************************************************)

checkMultipleDensity[newDensity_, indepDensityList_, indVarList_] :=
Module[{multipleCheck, constantFactors, printCMD},

  If[debugCheckMultipleDensity, printCMD = Print, Clear[printCMD],
      Clear[printCMD]];
  printCMD["debug code: CMD, Function: checkMultipleDensity,"<>
      " File: inchmude.m"];

  printCMD["At CMD IN, the current density being evaluated:"];
  printCMD[newDensity];
  printCMD["At CMD IN, the current list of independent densities"];
  printCMD[indepDensityList];
  printCMD["At CMD IN, the list of independent variables: ", indVarList];

  multipleCheck = Map[Factor, indepDensityList/newDensity];
  printCMD["At CMD 1, all densities in the independent density list have"<>
      " been divided by the current density being evaluated:"];
  printCMD[multipleCheck];

  constantFactors = Map[MemberQ[#, Apply[Alternatives ,{u[_][__],
      Derivative[__][_][__], Apply[Sequence, indVarList], t}],
      {0, Infinity}] &, multipleCheck];
  constantFactors = Thread[Or[constantFactors]];
  printCMD["At CMD 2, after Factor is applied to the division, True"<>
      " indicates that the division contains u[_] or a derivative and"<>
      " False indicates a multiple factor:"];
  printCMD[constantFactors];

  constantFactors = Position[constantFactors, False];
  printCMD["At CMD 3, constant factors are in the positions:"];
  printCMD[constantFactors];

  Clear[printCMD];

  Return[{Extract[indepDensityList, constantFactors],
      1/Extract[multipleCheck, constantFactors]}]
] (* end Module multipleDensity *)

(* ##########         Function: checkForDependency         ########## *)

(**********************************************************************)
(* checkForDependency[newDensity, indepDensityList, parameters]       *)
(* Purpose: To determine if a density is a linear combination of      *)
(*          independent densities                                     *)
(* Input:   The density currently being evaluated                     *)
(*          A list of densities known to be independent               *)
(*          A list of parameters                                      *)
(* Output:  If the density currently being evaluated is a linear      *)
(*          combination of independent densities, the linear          *)
(*          combination will be reported                              *)
(* Created: 29 January, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inchdepd.m                          *)
(* Last Modified:  18 March, 2008, 18:14 by DP at CSM                 *)
(**********************************************************************)

checkForDependency[newDensity_, indepDensityList_, parameters_,
  densORvarder_] :=
Module[{densityList, checkSolution, checkSolutionForZero, linearConstants,
    basisList, linearSys, printCFD},

  If[debugCheckForDependency, printCFD = Print, Clear[printCFD],
      Clear[printCFD]];
  printCFD["debug code: CFD, Function: checkForDependency,"<>
      " File: inchdepd.m"];

  printCFD["At CFD IN, the current ", If[densORvarder === den, "density",
    "variational derivative on the density"], " being evaluated:"];
  printCFD[newDensity];
  printCFD["At CFD IN, the current list of",
   If[densORvarder === var, " variational derivatives on all", ""],
   " independent densities"];
  printCFD[indepDensityList];
  printCFD["At CFD IN, the list of parameters includes ", parameters];

  (* Add the density being evaluated to the list of independent         *)
  (* densities.                                                         *)
  densityList = Append[indepDensityList, newDensity];
  printCFD["At CFD 1 a full list of "<>
      If[densORvarder === den, "densities:", "variational derivatives:"]];
  printCFD[densityList];

  (* Form a linear combination of all density with undetermined         *)
  (* coefficients, set up a system of undetermined coefficients and     *)
  (* solve the system.                                                  *)
  If[densORvarder === den, densityList = Map[List, densityList]];
  {linearSys, checkSolution} = checkLinearCombInd[densityList, p, parameters];
  checkSolution = Flatten[checkSolution];
  printCFD["At CFD 2, the linear combination produces the coefficient"<>
      " solution:"];
  printCFD[checkSolution];

  (* Isolate any linear dependencies and record the independent         *)
  (* densities that the current density in dependent on.                *)
  checkSolution = Cases[checkSolution, Rule[p[_], n1_] /; n1 =!= 0];
  printCFD["At CFD 3, the undetermined constants found in dependencies:"];
  printCFD[checkSolution];

  (* If the current density appears to be independent of all other      *)
  (* densities, check for partial dependence.                           *)
  If[checkSolution === {},
    partialDependence = checkForPartialDependence[linearSys];
    If[partialDependence === {},
      Clear[densityList, checkSolution, checkSolutionForZero, linearConstants,
          linearSys, printCFD];
      Return[{"no", {}}]
    ]; (* end If partialDependence *)

    (* A partial dependence exists.                                     *)
    basisList = partialDependence /. p[n1_] -> extr[indepDensityList, {n1}] /.
    extr -> Extract;
    printCFD["At CFD 4, the current density is dependent on this list of"<>
        " known independent densities:"];
    printCFD[basisList];

    Clear[densityList, checkSolution, checkSolutionForZero,  linearConstants,
        linearSys, printCFD];
    Return[{"partial", basisList}]
  ]; (* end If checkSolution *)

  (* Make a list of densities that the current density is dependent on  *)
  (* paired with linear constants so that the linear combination can be *)
  (* reconstructed.                                                     *)
  linearConstants = checkSolution /.
      Rule[n1_, Times[n2___, p[_]]] -> List[-n2, n1] /.
      Rule[n3_, p[_]] -> List[-1, n3];
  printCFD["At CFD 5, the linear constants for the basis densities:"];
  printCFD[linearConstants];

  constantOnNewDensity = p[Length[indepDensityList] + 1];
  printCFD["At CFD 6, the constant on the density being evaluated:"];
  printCFD[constantOnNewDensity];

  constantOnDepDensity = First[Union[Cases[Map[Take[#, -1] &, checkSolution  /.
      Rule -> List], p[_], {0, Infinity}]]];
  printCFD["At CFD 7, the constant of the dependent density:"];
  printCFD[constantOnDepDensity];

  constantCheck = If[constantOnDepDensity =!= constantOnNewDensity,
    positionDepDensity = constantOnDepDensity /. p[n1_] -> n1;
    Return[{"revise", {positionDepDensity,
        Extract[indepDensityList, {positionDepDensity}]}}];
  ]; (* end If constantOnDepDensity *)

  basisList = linearConstants /. p[n1_] -> extr[indepDensityList, {n1}] /.
      extr -> Extract;
  printCFD["At CFD 9, the current density is dependent on this list of"<>
      " known independent densities:"];
  printCFD[basisList];

  Clear[densityList, checkSolution, checkSolutionForZero,  linearConstants,
      linearSys, printCFD];

  Return[{"full", basisList}]
  ] (* end Module checkDependentDensity *)

(* ##########      Function: checkForPartialDependence       ########## *)

(**********************************************************************)
(* checkForPartialDependence[coefEquations]                           *)
(* Purpose: To find terms in a density that are equivalent to other   *)
(*          densities known to be independent.                        *)
(* Input:   A coefficient equation list                               *)
(* Output:  A list of coefficients that have dependencies.            *)
(* Created: 7 February, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inchpade.m                          *)
(* Last Modified:  18 March, 2008, 18:16 by DP at CSM                 *)
(**********************************************************************)

checkForPartialDependence[coefEquations_List] :=
Module[{matchingTerms, coefsInvolved, coefsAlone, coefsDepParts, getMultiples,
    printCFPD},

  If[debugCheckForPartialDependence, printCFPD = Print, Clear[printCFPD],
      Clear[printCFPD]];
  printCFPD["debug code: CFPD, Function: checkForPartialDependence,"<>
      " File: inchpade.m"];

  printCFPD["At CFPD IN, the system of coefficient equations:"];
  printCFPD[coefEquations];

  (* Look for undetermined coefficients that exist in combination with  *)
  (* other coefficients only.                                           *)
  matchingTerms = Cases[coefEquations, _Plus, {0, 3}];
  printCFPD["At CFPD 1, coefficient expressions with head Plus:"];
  printCFPD[matchingTerms];

  coefsInvolved = Union[Cases[matchingTerms, _p, {0, 3}]];
  printCFPD["At CFPD 2, a list of undetermined coefficients in expressions"<>
      " with head Plus:"];
  printCFPD[coefsInvolved];

  coefsAlone = Union[Map[
      Cases[coefEquations, Equal[#, 0], {0, 3}] &, coefsInvolved], Map[
      Cases[coefEquations, Equal[Times[__, #], 0], {0, 3}] &, coefsInvolved]];
  coefsAlone = Union[Cases[coefsAlone, _p, {0, 4}]];
  printCFPD["At CFPD 3, a list of undetermined coefficients that are in"<>
      " expressions with head Plus and in expressions alone:"];
  printCFPD[coefsAlone];

  coefsDepParts = Complement[coefsInvolved, coefsAlone];
  printCFPD["At CFPD 4, coefficients on terms in the denisty being"<>
      " evaluated that have been established as independent densities already:"];
  printCFPD[coefsDepParts];

  If[coefsDepParts =!= {},
    getMultiples = Solve[Map[Equal[#, 0] &, matchingTerms], coefsDepParts],
    getMultiples = {}
  ]; (* end If coefsDepParts *)
  printCFPD["At CFPD 5, the multiples on dependent terms:"];
  printCFPD[getMultiples];

  getMultiples = Flatten[getMultiples] /.
      Rule[n1_, Times[n2___, p[_]]] -> List[-n2, n1] /.
      Rule[n3_, p[_]] -> List[-1, n3];
  printCFPD["At CFPD 6, the multiples paired with their coefficient:"];
  printCFPD[getMultiples];

  getMultiples = If[MemberQ[getMultiples, _Rule, {0,Infinity}], {},
      getMultiples];
  printCFPD["At CFPD 7, the multiples are removed only if there is no"<>
      " coefficient to pair them with:"];
  printCFPD[getMultiples];

  Clear[matchingTerms, coefsInvolved, coefsAlone, coefsDepParts, printCFPD];

  Return[getMultiples];
  ] (* end Module checkForPartialDependence *)

(* ##########      Function: reportDensityEvaluation       ########## *)

(**********************************************************************)
(* reportDensityEvaluation[evaluationOfTerms, indepVars]              *)
(* Purpose: To print out a report if any total divergence terms or    *)
(*          divergence-equivalent terms exist.                        *)
(* Input:   True or False if the density is matched to the PDE        *)
(*          A list of total divergences and/or divergence-            *)
(*              equivalent terms                                      *)
(*          A list of independent variables                           *)
(* Output:  A report on the terms is printed, nothing is returned.    *)
(* Created: 7 February, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inrepdee.m                          *)
(* Last Modified:  11 March, 2008, 14:46 by DP at CSM                 *)
(**********************************************************************)

reportDensityEvaluation[evaluationOfTerms_List, indepVars_] :=
Module[{dword, dword2, lthinv = Length[indepVars], dx, dy, dz, dt, varx,
    vary, varz, vart, part1Res, totalDiv, divEquiv, currentTerm1,
    currentTerm2, currentTerm3, homotopyResult, lthCurrent, printRDE},

  If[debugReportDensityEvaluation, printRDE = Print, Clear[printRDE],
      Clear[printRDE]];
  printRDE["debug code: RDE, Function: reportDensityEvaluation,"<>
      " File: inrepdee.m"];

  printRDE["At RDE IN, a list containing terms that are total divergences"<>
      " and divergence equivalent:"];
  printRDE[evaluationOfTerms];
  printRDE["At RDE IN, the independent variables are ", indepVars];

  (* Initializations.                                                   *)
  dword = If[lthinv === 1, "total derivative", "divergence"];
  dword2 = If[lthinv === 1, dx, "Div"];
  dx = StyleForm[Subscript["D", "x"], FontSlant -> Italic];
  dy = StyleForm[Subscript["D", "y"], FontSlant -> Italic];
  dz = StyleForm[Subscript["D", "z"], FontSlant -> Italic];
  dt = StyleForm[Subscript["D", "t"], FontSlant -> Italic];
  varx = StyleForm["x", FontSlant -> Italic];
  vary = StyleForm["y", FontSlant -> Italic];
  varz = StyleForm["z", FontSlant -> Italic];
  vart = StyleForm["t", FontSlant -> Italic];

  {totalDiv, divEquiv} = evaluationOfTerms;

  Print["A term by term evaluation of the given density yields the"<>
      " following information:"];
  printRDE["At RDE 1, entering print loop for "<>dword<>"s."];

  While[totalDiv =!= {},
    currentTerm1 = First[totalDiv];
    currentTerm2 = pdeform[currentTerm1];
    printRDE["At RDE 2, the current terms being evaluated:"];
    printRDE[currentTerm1];

    If[! globalVerbose,
      Print["* The term ", currentTerm2, " is a " <> dword <> ".\n  The" <>
          " density can be simplified by moving this term into the flux."],

    (*else*)
      homotopyResult = pdeform[homotopyOperator[currentTerm1, indepVars,
          printStandAloneOutput -> False]];
      part1Res = If[lthinv === 1, homotopyResult, Part[homotopyResult, 1]];
      Print["* The term ", currentTerm2, " is a " <> dword <> ", that is\n"<>
          "    ", currentTerm2, " = ", dword2, If[lthinv === 1, "(", ""],
          homotopyResult, If[lthinv === 1, ")", ""], ".\n  Following the"<>
          " conservation law equation (1), if a total ", vart, "-derivative"<>
          "\n  is applied to this density term, it can be rewritten as "];
      Print["    ", dt, "(", dword2, If[lthinv === 1, "(", ""],
          homotopyResult, If[lthinv === 1, ")", ""], ") = ", If[lthinv === 1,
          Apply[Sequence, {dx, "(", dt, "(", homotopyResult, ")."}],
          Apply[Sequence, {dt, "(", dx, "(", part1Res, ") + ", dy, "(",
          Part[homotopyResult, 2], ")", If[lthinv === 2, ")",
          Apply[Sequence, {" + ", dz, "(", Part[homotopyResult, 3], "))"}]],
          "\n               = ", dx, "(", dt, "(", part1Res, ")) + ", dy, "(",
          dt, "(", Part[homotopyResult, 2], "))", If[lthinv === 3,
          Apply[Sequence, {" + ", dz, "(", dt, "(", Part[homotopyResult, 3],
          "))"}], ""]}], ""]];
      Print["  The term, ", currentTerm2, ", now written as a\n  ", varx, "-",
          If[lthinv === 2, Apply[Sequence, {" and ", vary, "-"}],
          If[lthinv === 3, Apply[Sequence, {", ", vary, "-, and ", varz,
          "-"}], ""]], "derivative, is removed from the density. \n  The"<>
          " term ", dt, "(", part1Res, ") is placed into the ",
          If[lthinv === 1, "", Apply[Sequence, {varx, "-component of the "}]],
          "flux", If[lthinv === 2, Apply[Sequence, {" and\n  the term ", dt,
          "(", Part[homotopyResult, 2], ") is placed into the ", vary,
          "-component of the flux"}], If[lthinv === 3, Apply[Sequence,
          {",\n  the term ", dt, "(", Part[homotopyResult, 2], ") is placed"<>
          " into the ", vary, "-component of the flux, and\n  the term ", dt,
          "(", Part[homotopyResult, 3], ") is placed into the ", varz,
          "-component of the flux"}], ""]], ".\n  If the given PDE is in"<>
          " evolution form, all ", vart, "-derivatives\n  can be replaced"<>
          " with their evolution equivalent from the PDE."]
    ]; (* end If globalVerbose *)

    totalDiv = If[totalDiv =!= {}, Drop[totalDiv, 1], totalDiv]
  ]; (* end While *)
  printRDE["At RDE 3, leaving print loop for "<>dword<>"s and entering"<>
      " print loop for "<>dword<>"-equivalences"];

  While[divEquiv =!={},
    currentTerm1 = First[divEquiv];
    currentTerm2 = pdeform[currentTerm1];
    printRDE["At RDE 4, the current terms being evaluated:"];
    printRDE[currentTerm1];

    lthCurrent = Length[currentTerm1];
    If[!globalVerbose,
      Print["* The terms ", Insert[currentTerm2, "and", lthCurrent], " are "<>
          dword <> "-equivalent.  The density can be simplified by moving"<>
          " these terms into the flux."],

    (*else*)
      homotopyResult = pdeform[homotopyOperator[Apply[Plus,
          Flatten[currentTerm1]], indepVars, printStandAloneOutput -> False]];
      part1Res = If[lthinv === 1, homotopyResult, Part[homotopyResult, 1]];
      currentTerm3 = Insert[currentTerm2, ", ", Range[2, lthCurrent - 2]];
      currentTerm3 = Insert[currentTerm3, ", and ", Length[currentTerm3]];
      Print["* The terms ", Apply[Sequence, currentTerm3], " are " <> dword <>
          "-\n  equivalent, that is,\n    ", Apply[Plus, currentTerm2],
          " = ", dword2, If[lthinv === 1, "(", ""], homotopyResult,
          If[lthinv === 1, ")", ""], ".\n  Following the conservation"<>
          " law equation, (1), if a ", StyleForm["t", FontSlant -> Italic],
          "-derivative" <> "\n  is applied to the sum of the " <> dword <>
          "-equivalent terms, they can be rewritten as "];
      Print["    ", dt, "(", dword2, If[lthinv === 1, "(", ""],
          homotopyResult, If[lthinv === 1, ")", ""], ") = ", If[lthinv === 1,
          Apply[Sequence, {dx, "(", dt, "(", homotopyResult, ")."}],
          Apply[Sequence, {dt, "(", dx, "(", part1Res, ") + ", dy, "(",
          Part[homotopyResult, 2], ")", If[lthinv === 2, ")", Apply[Sequence,
          {" + ", dz, "(", Part[homotopyResult, 3], "))"}]],
          "\n                 = ", dx, "(", dt, "(", part1Res, ")) + ", dy,
          "(", dt, "(", Part[homotopyResult, 2], "))", If[lthinv === 3,
          Apply[Sequence, {" + ", dz, "(", dt, "(", Part[homotopyResult, 3],
          "))"}], ""]}], ""]];
      Print["  The sum of the " <> dword <> "-equivalent terms,\n  ",
          Apply[Plus, currentTerm2], ", now written as a\n  ", varx, "-",
          If[lthinv === 2, Apply[Sequence, {" and ", vary, "-"}],
          If[lthinv === 3, Apply[Sequence, {", ", vary, "-, and ", varz, "-"}],
          ""]], "derivative, is removed from the density. \n  The term ", dt,
          "(", part1Res, ") is placed into the ", If[lthinv === 1, "",
          Apply[Sequence, {varx, "-component of the "}]], "flux",
          If[lthinv === 2, Apply[Sequence, {" and\n  the term ", dt, "(",
          Part[homotopyResult, 2], ") is placed into the ", vary,
          "-component of the flux"}], If[lthinv === 3, Apply[Sequence,
          {",\n  the term ", dt, "(", Part[homotopyResult, 2], ") is placed"<>
          " into the ", vary, "-component of the flux, and\n  the term ", dt,
          "(", Part[homotopyResult, 3], ") is placed into the ", varz,
          "-component of the flux"}], ""]], ".\n  If the given PDE is in"<>
          " evolution form, all ", vart, "-derivatives\n  can be replaced"<>
          " with their evolution equivalent from the PDE."]
    ]; (* end If global Verbose *)
    divEquiv = If[divEquiv =!= {}, Drop[divEquiv, 1], divEquiv]
  ]; (* end While *)
  printRDE["At RDE 5, leaving print loop for "<>dword<>"-equivalences"];

  Clear[dword, dword2, totalDiv, divEquiv, currentTerm1, currentTerm2,
    currentTerm3, homotopyResult, lthCurrent, printRDE]

] (* end Module reportDensityEvaluation *)

(* ##########          Function: reportEquivalence         ########## *)

(**********************************************************************)
(* reportEquivalence[complete, denNo, currentDenBeingEval,            *)
(*     revisedDensity, indepDensityList, resultFromEquivCheck,        *)
(*     indepVars, numDepVars, allParams]                              *)
(* Purpose: To print a report detailing pieces of a density that are  *)
(*     equivalent to established independent densities and to report  *)
(*     if the density is independent.                                 *)
(* Input:   A note stating whether a density has any equivalent pieces*)
(*          The number of the density given in the original list      *)
(*          The full density being evaluated                          *)
(*          The part of the density not already found dependent       *)
(*          The dependent pieces, if any have been found              *)
(*          A list of independent variables                           *)
(*          The number of dependent variables                         *)
(* Output:  The reports are printed to the screen                     *)
(*          Any independent piece is returned                         *)
(* Created: 8 February, 2008 by DP at CSM                             *)
(* Code is in File:  conservationlaws/inrepequ.m                      *)
(* Last Modified:  23 May, 2008, 14:39 by DP at CSM                   *)
(**********************************************************************)

reportEquivalence[complete_, denNo_, currentDenBeingEval_, revisedDensity_,
    indepDensityList_List, resultFromEquivCheck_List, indepVars_List,
    numDepVars_, allParams_] :=
Module[{lthInd, dword1, dword2, printAdd1, printAdd2, equivDiff, lthequden,
    mixedList, indepPart, denomParamCheck, newResultFromEquivCheck, es,
    positionIndDen, indepDensFromList, indepDens, densityPart, n1, n2, n3, n4,
    revisedDenList, varDerOnRevisedList, strippedResultFromEquivCheck,
    positionEquivDensity, lst, positionEquivDensityWOcoefs,
    positionConstantVarDers, positionZerosInVarDer, equivDensities, printRE},

  If[debugReportEquivalence, printRE = Print, Clear[printRE], Clear[printRE]];
  printRE["debug code: RE, Function: reportEquivalence, File: inrepequ.m"];

  printRE["At RE IN, the original density no. ", denNo, " being evaluated:"];
  printRE[currentDenBeingEval];
  printRE["At RE IN, parts of the density have been found to be trivial or"<>
     " dependent and have been removed, leaving the revised density:"];
  printRE[revisedDensity];
  printRE["At RE IN, all or part or none of the density being evaluated is"<>
      " equivalent to a linear combination of independent densities: ",
      complete];
  printRE["At RE IN, the result from the equivalence check:"];
  printRE[resultFromEquivCheck];
  printRE["At RE IN, the list of independent densities contains:"];
  printRE[indepDensityList];
  printRE["At RE IN, the independent variable list contains ", indepVars];
  printRE["At RE IN, the number of dependent variables is ", numDepVars];
  printRE["At RE IN, the list of parameters contains ", allParams];

  (* Initialiaztions *)
  lthInd = Length[indepVars];
  dword1 = If[lthInd === 1, "total derivative", "divergence"];
  dword2 = If[lthInd === 1, StyleForm[Subscript["D", "x"],
      FontSlant -> Italic], "Div"];

  printAdd1 := Block[{},
    Print["  The difference between Density no. ", denNo, " and the"<>
        " equivalent\n  established independent density multiplied by the"<>
        " factor given is"];
    Print["  ", pdeform[equivDiff], " = ", dword2, "[",
        pdeform[homotopyOperator[equivDiff, indepVars,
        printStandAloneOutput -> False]], "]."];
    Print["  Since the difference is a "<> dword1 <>
        ", the two densities are equivalent."]
  ]; (* end Block *)

  printAdd2 := Block[{},
    Map[Print["  The difference between the term"<>
        If[lthequden === 1, " ", "s "], pdeform[Part[#, 1]],
        " and "<>If[lthequden === 1, "its", "their"]<>
        " equivalent independent density is \n  ",
       pdeform[Expand[Part[#, 1] - Apply[Times, Part[#, 2]]]], " = ",
       dword2, "[",
       pdeform[homotopyOperator[Expand[Part[#, 1] - Apply[Times, Part[#, 2]]],
       indepVars, printStandAloneOutput -> False]], "]."] &, mixedList];
    Print["  Since all differences are "<>dword1<>"s, each\n  set of"<>
        " terms is eqivalent to an established independent density."]
  ]; (* end Block *)

  If[complete === "no",
    printRE["At RE 1, making a report when no part of the density is"<>
        " an equivalence."];
    indepPart = newNormalize[First[revisedDensity], {}];
    printRE["At RE 2, the normalized independent density is:"];
    printRE[indepPart];

    Print["The renormalized density, written as"];
    Print["  ", pdeform[indepPart]];
    Print["  has been found to be independent of all established"<>
        " independent densities.  It is added to the list of"<>
        " independent densities."];

   indepPart = {indepPart, Part[revisedDensity, 2],
        Table[variationalDerivativeMultiD[indepPart, i1,  indepVars],
        {i1, 1, numDepVars}]};

    Clear[lthind, dword1, dword2, printRE];

    Return[indepPart]
  ]; (* end If complete === "no" *)

  (* Simplify variational derivatives from equivalent densitys that     *)
  (* have parameters in their denominators.                             *)
  denomParamCheck = Union[Map[Denominator[Part[#, 2]] &,
      Factor[resultFromEquivCheck]]];
  printRE["At RE 3, variational derivatives coming from the equivalence"<>
      " check that have parameters in their denominators:"];
  printRE[denomParamCheck];

  If[MemberQ[denomParamCheck, Apply[Alternatives, allParams], {0, Infinity}],
    newResultFromEquivCheck = resultFromEquivCheck /.
         {n1_, {n2_}} -> {n1, {es[n2]}} /. es -> expressionSimplify,
  (* else *)
    newResultFromEquivCheck = resultFromEquivCheck
  ]; (* end If MemberQ *)
  printRE["At RE 4, the result from the equivalence check:"];
  printRE[newResultFromEquivCheck];

  (* Match the variational derivatives returned as equivalent to the    *)
  (* corresponding equivalent densities in the independent density list.*)
  positionIndDen = Map[Position[indepDensityList, Part[#, 2]] &,
      resultFromEquivCheck];
  printRE["At RE 5, the independent densities can be found at positions:"];
  printRE[positionIndDen];

  indepDensFromList = Map[First, Extract[indepDensityList,
     Map[Take[#, 1] &, Flatten[positionIndDen, 1]]]];
  printRE["At RE 6, the equivalent independent densities are:"];
  printRE[indepDensFromList];

  (* Match the variational derivatives returned as equivalent to the    *)
  (* corresponding equivalent expressions in the revised density list.  *)
  indepDens =  Thread[List[Map[First, newResultFromEquivCheck],
      indepDensFromList]];
  printRE["At RE 7, the equivalent independent densities with the"<>
      " multipliers are:"]; printRE[indepDens];

  densityPart = Expand[Part[revisedDensity, 1]];
  revisedDenList = If[Head[densityPart] === Plus, Apply[List, densityPart],
      {densityPart}];
  printRE["At RE 8, the revised density with all terms put into a list:"];
  printRE[revisedDenList];

  varDerOnRevisedList = Map[Expand[Table[variationalDerivativeMultiD[
      #, i1, indepVars], {i1, 1, numDepVars}]] &,  revisedDenList];
  printRE["At RE 9, a list of variational derivatives corresponding to"<>
      " the list of revised density terms:"];
  printRE[varDerOnRevisedList];

  strippedResultFromEquivCheck = Map[stripper[#, allParams] &,
      Map[Part[#, 2] &, newResultFromEquivCheck], {2}];
  printRE["At RE 10, the terms in the variational derivative returned from"<>
      " the equivalence check with numeric coefficients stripped:"];
  printRE[strippedResultFromEquivCheck];

  (* IMPORTANT!                                                         *)
  (* The next function matches the terms from the variational           *)
  (* derivative of the known independent density to terms in the        *)
  (* variational derivative of the revised density.  The function will  *)
  (* not work if there are any parameters that have not been declared   *)
  (* in the parameters list.                                            *)
  positionEquivDensity = Map[Position[varDerOnRevisedList,
      Times[n1___, #] /; (Not[MemberQ[{n1}, u[_][__], {0, Infinity}]] &&
      Not[MemberQ[{n1}, Derivative[__][u[_]][__], {0, Infinity}]] &&
      Not[MemberQ[{n1}, Apply[Alternatives, allParams], {0, Infinity}]] &&
      (MemberQ[#, u[_][__], {0, Infinity}] ||
      MemberQ[#, Derivative[__][u[_]][__], {0, Infinity}] ||
      MemberQ[#, Apply[Alternatives, allParams], {0, Infinity}])), {0, 3}] &,
      strippedResultFromEquivCheck, {3}] /.
      {{n2_Integer, n3_Integer, n4___Integer}} -> {n2, n3, n4};
  printRE["At RE 11, the positions where terms with coefficients match in"<>
      " the variational derivatives in RE 9 and RE 10:"];
  printRE[positionEquivDensity];

  positionEquivDensity = Apply[lst, positionEquivDensity, {4}] /.
      List[n1__lst] -> Sequence[n1] /. lst -> List;
  printRE["At RE 12, the positions list has been adjusted for cases where"<>
      " more than one match takes place:"];
  printRE[positionEquivDensity];

  positionEquivDensityWOcoefs = Map[Position[varDerOnRevisedList, # /;
      Head[#] =!= Times && (MemberQ[#, u[_][__], {0, Infinity}] ||
      MemberQ[#, Derivative[__][u[_]][__], {0, Infinity}] ||
      MemberQ[#, Apply[Alternatives, allParams], {0, Infinity}]), {0, 2}] &,
      strippedResultFromEquivCheck, {3}] /.
      {{n1_Integer, n2_Integer}} -> {n1, n2};
  printRE["At RE 13, the positions where terms without coefficients match in"<>
      " the variational derivatives in RE 9 and RE 10:"];
  printRE[positionEquivDensityWOcoefs];

  positionConstantVarDers = Map[Position[varDerOnRevisedList, n1_ |
      Times[n1_, _Symbol] /; (NumericQ[n1] && n1 =!= 0 && # =!= 0 &&
      Not[MemberQ[{#}, u[_][__], {0, Infinity}]] &&
      Not[MemberQ[{#}, Derivative[__][u[_]][__], {0, Infinity}]] &&
      Not[MemberQ[{#}, Apply[Alternatives, allParams], {0, Infinity}]]),
     {0, 2}] &, strippedResultFromEquivCheck, {3}] /.
     {{n2_Integer, n3_Integer, n4___Integer}} -> {n2, n3, n4};
  printRE["At RE 14, the positions where there are matching constants in"<>
      " the variational derivatives in RE 9 and RE 10:"];
  printRE[positionConstantVarDers];

  positionEquivDensity = MapThread[List, {positionEquivDensity,
      positionEquivDensityWOcoefs, positionConstantVarDers}, 2];
  positionEquivDensity = Apply[Sequence, positionEquivDensity, {3}];
  printRE["At RE 15, the positions with coefficients, without coefficients"<>
      " and with constants have been combined into a single list:"];
  printRE[positionEquivDensity];

  positionEquivDensity = DeleteCases[positionEquivDensity, {}, {0, Infinity}];
  printRE["At RE 16, all empty lists have been removed:"];
  printRE[positionEquivDensity];

  If[numDepVars > 1,
    positionZerosInVarDer = Position[varDerOnRevisedList, 0, {0, 2}];
    printRE["At RE 16, the positions where zero components exist:"];
    printRE[positionZerosInVarDer];

    positionZerosInVarDer = Table[Cases[positionZerosInVarDer, {_, i1},
        {0, Infinity}], {i1, 1, numDepVars}];
    printRE["At RE 17, the positions where zero components exist organized"<>
        " by component:"];
    printRE[positionZerosInVarDer];

    positionEquivDensity = Map[Thread[List[#, positionZerosInVarDer]] &,
        positionEquivDensity, {1}];
    printRE["At RE 18, variational derivative list with zero components"<>
        " added:"];
    printRE[positionEquivDensity];

    positionEquivDensity = Apply[Sequence, positionEquivDensity, {3}];
    printRE["At RE 19, variational derivative list after an adjustment to"<>
          " the braces:"];
    printRE[positionEquivDensity];
  ]; (* end If numDepVars *)

  positionEquivDensity = positionEquivDensity /. List[n1_Integer, __] -> {n1};
  printRE["At RE 20, the first number for each position has been retained -"<>
      " have been discarded:"];
  printRE[positionEquivDensity];

  positionEquivDensity = Apply[Intersection, positionEquivDensity, {1}];
  printRE["At RE 21, terms in the density being evaluated have been matched"<>
      " to the variational derivatives of independent densities:"];
  printRE[positionEquivDensity];

  equivDensities = Apply[Plus, Map[Extract[revisedDenList, #] &,
      positionEquivDensity], {1}];
  printRE["At RE 22, the equivalent densities found in the revised"<>
      " density list:"];
  printRE[equivDensities];

  printRE["At RE 23, making a report when a "<>complete<> " density is"<>
      " an equivalence."];
  Print["A check for equivalence to other densities yields the following"<>
      " information."];

  If[Length[newResultFromEquivCheck] === 1,
    If[complete === "full",
      Print["* Density no. ", denNo, " is equivalent to the established"<>
          " independent density"],
      Print["* The terms ", pdeform[First[equivDensities]], " in"<>
          " Density no. ", denNo, "\n  are equivalent to the established"<>
          " independent density"]
    ]; (* end If complete === "full" *)
    Print["  ", pdeform[First[indepDensFromList]], " multiplied by ",
        Part[newResultFromEquivCheck, 1, 1]];
    If[globalVerbose,
      equivDiff = Expand[First[equivDensities] -
          Apply[Times, First[indepDens]]];
      printRE["At RE 24, the difference between the independent density"<>
          " and the equivalent density:"];
      printRE[equivDiff];
      printAdd1
    ]; (* end If globalVerbose *)

    If[complete === "full",
      Print["The evaluation of Density no. ", denNo, " is complete. It"<>
          " has no independent part."]],

  (* else newResultFromEquivCheck > 1 *)
    mixedList = Thread[List[equivDensities, indepDens]];
    printRE["At RE 25, a mixed list of equivalent densities followed by"<>
        " independent densities:"];
    printRE[mixedList];
    lthequden = If[Head[First[equivDensities]] === Plus,
        Length[equivDensities], 1];
    Print["* Density no. ", denNo, " is equivalent to a linear combination"<>
        " of established independent densities:"];
    Map[Print["  The term"<> If[lthequden === 1, "", "s"]<> " in\n  "<>
        "", pdeform[Part[#, 1]], " "<>If[lthequden === 1, "is", "are"] <>
        " equivalent to the independent density\n  ",
        pdeform[Part[#, 2, 2]], " multiplied by ", Part[#, 2, 1],
        "."] &, mixedList];
    If[globalVerbose,
      equivDiff = Expand[Thread[Plus[-equivDensities,
          Apply[Times, indepDens, {1}]]]];
      printRE["At RE 26, a list of differences between the independent"<>
          " density and its equivalent density:"];
      printRE[equivDiff];
      printAdd2
    ]; (* end If globalVerbose *)
    If[complete === "full",
      Print["The evaluation of Density no. ", denNo, " is complete. It"<>
          " has no independent part."]]
  ]; (* end If Length[newResultFromEquivCheck] *)

  If[complete === "partial",
    indepPart = Expand[First[revisedDensity] - Apply[Plus, equivDensities]];
    printRE["At RE 27, the independent part of the density is:"];
    printRE[indepPart];

    indepPart = newNormalize[indepPart, {}];
    printRE["At RE 28, the normalized independent density is:"];
    printRE[indepPart];

    Print["* All equivalent terms are removed.  The renormalized"<>
        " Density no. ", denNo, " now consists of"];
    Print["  ", pdeform[indepPart], "."];
    Print[ "The renormalized density, written as"];
    Print["  ", pdeform[indepPart]];
    Print["  has been found to be independent of all established"<>
        " independent densities.  It is added to the list of"<>
        " independent densities."];
    indepPart = {indepPart, Part[currentDenBeingEval, 2],
        Table[variationalDerivativeMultiD[indepPart, i1,  indepVars],
        {i1, 1, numDepVars}]};
    Clear[lthInd, dword1, dword2, printAdd1, printAdd2, equivDiff, lthequden,
        mixedList, denomParamCheck, newResultFromEquivCheck, es,
        positionIndDen, indepDensFromList, indepDens, densityPart, n1, n2, n3,
        revisedDenList, varDerOnRevisedList, strippedResultFromEquivCheck,
        positionEquivDensity, lst, positionEquivDensityWOcoefs, n4, printRE,
        positionConstantVarDers, positionZerosInVarDer, equivDensities];
    Return[indepPart],

  (* else *)
    Clear[lthInd, dword1, dword2, printAdd1, printAdd2, equivDiff, lthequden,
        mixedList, indepPart, denomParamCheck, newResultFromEquivCheck, es,
        positionIndDen, indepDensFromList, indepDens, densityPart, n1, n2, n3,
        revisedDenList, varDerOnRevisedList, strippedResultFromEquivCheck,
        positionEquivDensity, lst, positionEquivDensityWOcoefs, n4, printRE,
        positionConstantVarDers, positionZerosInVarDer, equivDensities];

    Return[{}]
  ]; (* end If complete === "partial" *)

] (* end Module reportEquivalence *)

(* ##########          Function: printDensities            ########## *)

(**********************************************************************)
(* printDensities[densityList, compatibilityList, printcom]           *)
(* Purpose: To print out a list of densities                          *)
(* Input:   A list of densities                                       *)
(*          A list of compatibilities                                 *)
(*          A print condition for compatibilities                     *)
(* Output:  None.  All output is printed to the screen                *)
(* Created: 3 May 2007 by DP at CSM                                   *)
(* Code is in File:  independence/inpriden.m                          *)
(* Last Modified:  5 March, 2007, 16:00 by DP at CSM                  *)
(**********************************************************************)

printDensities[densityList_List, compatibilityList_List, printcom_] :=
  Module[{numOfDensities, i},

  numOfDensities = Length[densityList];
  Print["A Listing of All Densities Given in the Data File"];
  For[i = 1, i <= numOfDensities, i++,
    Print["Density No. ", i, " is given as"];
    Print["  ", pdeform[Part[densityList, i]]];
    If[Part[compatibilityList, i] === {},
      printcom["  There are no conditions on any parameters for this density."];,
    (* else *)
      printcom["  This density has the condition"<>
      If[Length[Part[compatibilityList, i]] === 1, "", "s"] <> " on the"<>
          " parameters:"];
      printcom["  ", Part[compatibilityList, i]]
    ]; (* end If Part *)
  ]; (* end For *)

  Clear[numOfDensities, i]
] (* end Module *)

(* ##########        Function: checkLinearCombInd          ########## *)

(**********************************************************************)
(* checkLinearCombInd[expnList, constantHead, params                  *)
(* Purpose: To assign undetermined coefficients to a list of          *)
(*          expressions or terms, form a linear combination of the    *)
(*          expressions or terms, form a system of coefficients and   *)
(*          solve the system.                                         *)
(* Input:   A list of terms or expressions                            *)
(*          The head for the undetermined coefficients                *)
(*          A list of parameters in the problem                       *)
(* Output:  The solution for the system of undetermined coefficients  *)
(* Created: 28 January, 2008 by DP at CSM                             *)
(* Code is in File:  independence/inchlico.m                          *)
(* Last Modified:  18 March, 2008, 16:15 by DP at CSM                 *)
(**********************************************************************)

checkLinearCombInd[expnList_List, constantHead_Symbol, params_List] :=
Module[{linearComb, linearSystem, coefficientList, coefficientSolns,
    printCLCI},

  If[debugCheckLinearCombInd, printCLCI = Print, Clear[printCLCI],
      Clear[printCLCI]];
  printCLCI["debug code: CLCI, Function: checkLinearCombInd,"<>
      " File: inchlico.m"];

  printCLCI["At CLCI IN, a list of expressions to be checked for linear"<>
      " independence:"];
  printCLCI[expnList];
  printCLCI["At CLCI IN, the head for the constant term is ", constantHead];
  printCLCI["At CLCI IN, the list of parameters is ", params];

  If[Not[MemberQ[expnList, _constantHead, {0, Infinity}]],
    (* Set up a list of coefficients.                                   *)
    coefficientList = Table[constantHead[i1], {i1, 1, Length[expnList]}];

    (* Set up a linear combination of terms.                            *)
    linearComb = Expand[Thread[Times[expnList, coefficientList]]];
    printCLCI["At CLCI 1, the list of expressions paired with undetermined"<>
        " coefficents:"];
    printCLCI[linearComb],

  (* else *)
    coefficientList = Union[Cases[expnList, _p, {0, Infinity}]];
    linearComb = Transpose[expnList];
    printCLCI["At CLCI 1a, the list of expressions paired with undetermined"<>
        " coefficents:"];
    printCLCI[linearComb]
  ]; (* end If Not[MemberQ] *)

  linearComb = Expand[Apply[Plus, linearComb, {1}]];
  printCLCI["At CLCI 2, all terms combined into expression(s) with"<>
      " undetermined coefficients:"];
  printCLCI[linearComb];

  (* Formulate a system of coefficient expressions by matching terms.   *)
  linearSystem = Map[extractSystemForCoefficients[#, coefficientList,
      params]&, linearComb];
  linearSystem = Flatten[linearSystem];
  printCLCI["At CLCI 3, a system of expressions of undetermined"<>
      " coefficients:"];
  printCLCI[linearSystem];

  (* Remake the coefficient expressions into coefficient equations.     *)
  linearSystem = Map[Equal[#, 0] &, Flatten[linearSystem]];
  printCLCI["At CLCI 4, a system of equations of undetermined coefficients:"];
  printCLCI[linearSystem];

  (* Solve the system of equations for the constants, c[i]. *)
  coefficientSolns = Solve[linearSystem, coefficientList];
  printCLCI["At CLCI 5, the solution to the system of equations of"<>
      " undetermined coefficients:"];
  printCLCI[coefficientSolns];

  Clear[linearComb, coefficientList, printCLCI];

  Return[{linearSystem, coefficientSolns}]
  ] (* end Module checkLinearCombInd *)

(* #################################################################### *)
(*                                                                      *)
(* ##########         End of Independence Algorithm          ########## *)
(*                                                                      *)
(* #################################################################### *)

(* ##########        Function: matchDensityWithFlux        ########## *)

(**********************************************************************)
(* matchDensityWithFlux[finalRhoList, evolutionRules,                 *)
(*     numDepVariables, indepVariableList, listOfParams, userFormRho] *)
(* Purpose: To match each density with its flux.  The flux will be    *)
(*          using homotopy operaters.                                 *)
(* Input:   A list of densities with their compatibility conditions   *)
(*          Evolution rules for the PDE                               *)
(*          The number of dependent variables                         *)
(*          A list of independent variables                           *)
(*          A list of parameters                                      *)
(*          The form of a density given by the user (if given)        *)
(* Output:  A list of fluxes corresponding to the densities in        *)
(*              finalRhoList                                          *)
(* Created: 26 August 2006 by DP at home                              *)
(* Code is in File:  conservationlaws/nwmtdefl.m                      *)
(* Last Modified:  11 August, 2008, 10:09 by DP at CSM                *)
(**********************************************************************)

(* Function for divergence in 1, 2, or 3 dimensions.                     *)
div[listOfJ_] :=
  If[Head[listOfJ] =!= List,
    D[listOfJ, x],
    If[Length[listOfJ] === 2,
      D[Part[listOfJ, 1], x] + D[Part[listOfJ, 2], y],
      D[Part[listOfJ, 1], x] + D[Part[listOfJ, 2], y] + D[Part[listOfJ, 3], z]
    ] (* end If *)
  ]; (* end If *)

matchDensityWithFlux[finalRhoList_, evolutionRules_, numDepVariables_,
    indepVariableList_, listOfParams_List, userFormRho_] :=
Module[{tDerivativesOfRho, i1, rpl, tDerivativesOfRhoWOt, m1, m2, m3, m4,
    m5, jList, printMDWF},

  If[debugMatchDensityWithFlux, printMDWF = Print, Clear[printMDWF],
      Clear[printMDWF]];

  printMDWF["debug code: MDWF, Function: matchDensityWithFlux,"<>
      " File: nwmtdefl.m"];

  printMDWF["At MDWF IN, the list of densities paired with their"<>
      " compatibility conditions):"];
  printMDWF[finalRhoList];
  printMDWF["At MDWF IN, evolution rules for the PDE consist of"];
  printMDWF[evolutionRules];
  printMDWF["At MDWF IN, the number of dependent variables is ",
      numDepVariables];
  printMDWF["At MDWF IN, the list of independent variables is ",
      indepVariableList];
  printMDWF["At MDWF IN, all parameters in the original PDE: ", listOfParams];
  printMDWF["At MDWF IN, if the user is testing a density, the density is:"];
  printMDWF[userFormRho];

  (* Find the t-derivatives of each rho.                                  *)
  tDerivativesOfRho = Map[-D[#, t] &, Table[Part[finalRhoList, i, 1],
      {i, 1, Length[finalRhoList]}]];
  printMDWF["At MDWF 1, a list of t-derivatives of the densities:"];
  printMDWF[tDerivativesOfRho];

  (* Replace t-derivatives using the evolution rules.                     *)
  tDerivativesOfRho = Map[replaceOnTheEquation[evolutionRules, #,
      indepVariableList] &, tDerivativesOfRho];
  printMDWF["At MDWF 2, t-derivatives after evolution rules have been"<>
      " applied:"];
  printMDWF[tDerivativesOfRho];

  (* Apply compatibility rules on parameters found in the general solver. *)
  tDerivativesOfRho = Thread[rpl[tDerivativesOfRho,
      Table[Part[finalRhoList, i, 2], {i, 1, Length[finalRhoList]}]]] /.
      rpl -> ReplaceRepeated;
  printMDWF["At MDWF 3, t-derivatives after compatibility rules have"<>
      " been applied:"];
  printMDWF[tDerivativesOfRho];

  (* The homotopy operator will take t as an independent space variable; *)
  (* t must be removed as an independent variable.                       *)
  tDerivativesOfRhoWOt = Flatten[Expand[tDerivativesOfRho /.
      {Derivative[m1__, 0][m2_][m3__, t] -> Derivative[m1][m2][m3],
      u[m4_][m5__,t] -> u[m4][m5]}]];

  printMDWF["At MDWF 4, t-derivatives of densities to be fed to the"<>
      " homotopy operator:"];
  printMDWF[tDerivativesOfRhoWOt];

  (* Apply the homotopy operator to get the flux. *)
  jList = Map[homotopyOperator[#, indepVariableList, numDepVariables] &,
      tDerivativesOfRhoWOt];
  jList = Expand[Factor[jList]];
  printMDWF["At MDWF 5, a list of fluxes returned by the homotopy operator:"];
  printMDWF[jList];

  jList = jList /.
      {Derivative[m1__][u[m2_]][m3__] -> Derivative[m1, 0][u[m2]][m3, t],
      u[m4_][m5__] -> u[m4][m5,t]};
  printMDWF["At MDWF 6, a list of fluxes after the t-variable has been"<>
      " replaced:"];
  printMDWF[jList];

  (* Clear all local variables not being returned.                      *)
  Clear[tDerivativesOfRho, i1, tDerivativesOfRhoWOt, m1, m2, m3, m4, m5,
      printMDWF];

  Return[jList]
]; (* end Module matchDensityWithFlux *)

(* #################################################################### *)
(*                                                                      *)
(* ##########          Start of Homotopy Operator            ########## *)
(*                                                                      *)
(* #################################################################### *)

(* ##########         Function: homotopyStripper           ########## *)

(**********************************************************************)
(* homotopyStripper[givenexpr, indVarsList, paramswithweight]         *)
(* Purpose: Cancels the numerical factors of each term of the         *)
(*          argument and puts these into a list after cancellation    *)
(* Input:   An expression                                             *)
(*          A list of independent variables                           *)
(*          A list of weighted parameters is optional                 *)
(* Output:  A list of the terms of the expression with all constant   *)
(*          coefficients removed.                                     *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whlstrip.m and conservationlaws/dpnstrip.m          *)
(* Code is in File:  newhomotopyoper/honstrip.m                       *)
(* Major Changes Made: 22 May, 2006, 14:30 by DP at CSM               *)
(* Last Modified:  10 March, 2007, 12:31 by DP at CSM                 *)
(**********************************************************************)

homotopyStripper[givenexpr_, indVarsList_, paramswithweight_:{}] :=
Module[{expnList, expnFnList, coefficientList, singleNoCoefficientList,
    strippedTermsList, expandExpr, n1, rules1, rules2, rules3, printHST},

  If[debugHomotopyStripper, printHST = Print, Clear[printHST],
      Clear[printHST]];

  printHST["debug code: HST, Function: homotopyStripper, File: honstrip.m"];

  (* If input expression is zero, deal with it separately.              *)
  If[givenexpr === 0, strippedTermsList = {0};
  Return[strippedTermsList]];

  (* Input expression must be in expanded form.                         *)
  expandExpr = Expand[givenexpr];

  (* Create a list of terms in the given expression.                    *)
  expnList = If[Head[expandExpr] === Plus, Apply[List, expandExpr],
      {expandExpr}, {expandExpr}];
  printHST["At HST 1, a list of terms in the given expression is:"];
  printHST[expnList];

  (* Separate the coefficients and parameters from the terms            *)
  expnFnList = Apply[Power, Map[FactorList, expnList], {2, 2}];
  printHST["At HST 2, each term in expnList has been changed into a"<>
      " list of factors:"];
  printHST[expnFnList];

  (* Create a list of coefficients - not including any weighted         *)
  (* parameters. Two rules are developed to remove all unwanted parts.  *)
  rules1 = _[n1_, ___] /; MemberQ[indVarsList, n1] -> {};
  rules2 = Map[Rule[#, {}] &, paramswithweight];
  rules3 = Map[Rule[#, {}] &, indVarsList];
  printHST["At HST 4, the rules for removing coefficients and parameters:"];
  printHST["rules1 = ", rules1, "\nrules2 = ", rules2 ,"\nrules3 = ", rules3];

  coefficientList = expnFnList /. rules1;
  coefficientList = coefficientList /. rules2;
  coefficientList = coefficientList /. rules3;
  printHST["At HST 4, a list of coefficients where all unwanted parts"<>
      " have been changed to {}:"];
  printHST[coefficientList];

  (* The {} is used first to avoid getting zeros in denominators when   *)
  (* rational terms are given in the problem.                           *)
  coefficientList = coefficientList /. {} -> 1;
  printHST["At HST 5, a list of coefficients where {} is changed to 1:"];
  printHST[coefficientList];

  (* Combine the coefficients for each term.                            *)
  singleNoCoefficientList = Apply[Times, coefficientList, {1, 1}];
  printHST["At HST 6, a combined coefficient list is:"];
  printHST[singleNoCoefficientList];

  (* Form a list of terms without coefficients, but including weighted  *)
  (* parameters.                                                        *)
  strippedTermsList = If[Length[expnFnList] > 1,
      Thread[Times[expnList, 1/singleNoCoefficientList]],
      expnList/singleNoCoefficientList];
  strippedTermsList = Factor[strippedTermsList];
  printHST["At HST 7, a list of stripped terms is:"];
  printHST[strippedTermsList];

  Clear[expnList, expnFnList, coefficientList, singleNoCoefficientList,
    expandExpr, n1, rules1, rules2, rules3, printHST];

  Return[strippedTermsList];
]; (* end Module homotopyStripper *)

(* ##########          Function: integrabilityTest           ########## *)

(************************************************************************)
(* integrabilityTest1D[expression, dependentVar, listOfIndepVars,       *)
(*                     (optional) parameters]                           *)
(* Purpose:  To determine if the expression is integrable by applying   *)
(*           the zero-Euler operator.  If the expression is not         *)
(*           integrable and parameters have been given, then the        *)
(*           function will try to identify values for the parameters    *)
(*           to make the expression integrable.                         *)
(* Input:    An expression                                              *)
(*           A number for the dependent variable                        *)
(*           A list of independent variables                            *)
(*           A list of parameters (optional)                            *)
(* Output:   A  partial derivative of the expression with respect to    *)
(*           terms over the jet space                                   *)
(*           The order of the expression with respect to the            *)
(*           independent variable                                       *)
(* Created:  22 March, 2006 by DP                                       *)
(* Code is in File:  newhomotopyoper/hointest.m                         *)
(* Last Modified: 10 March, 2008 at 14:46 by DP                         *)
(************************************************************************)

integrabilityTest[expression_, noOfDepVar_, listOfIndepVars_,
    parameters_:{}] :=
Module[{j, variationalDerivativeResult, testBasis},

  If[debugIntegrabilityTest, printIT = Print, Clear[printIT], Clear[printIT]];
  printIT["debug code: IT, Function: integrabilityTest,"<>
      " File: integrabilityTest.m"];

  printIT["At IT IN, the given expression is:"];
  printIT[expression];
  printIT["At IT IN, the number of dependent variables is ", noOfDepVar];
  printIT["At IT IN, the list of independent variables is ", listOfIndepVars];
  printIT["At IT IN, parameters given to the function include:"];
  printIT[parameters];

  (* Find the zero-Euler operator for the expression for each        *)
  (* component of the dependent variable.                               *)
  variationalDerivativeResult = Table[variationalDerivativeMultiD[
      expression, j, listOfIndepVars], {j, 1, noOfDepVar}];
  printIT["At IT 2, results from applying the zero-Euler operator to"<>
      " each component:"];
  printIT[variationalDerivativeResult];

  testBasis = Table[0, {j, 1, noOfDepVar}];
  If[variationalDerivativeResult === testBasis,
    printIT["At IT 3, the zero-Euler operator applied to the expression"<>
        " is zero for all components.  This expression is integrable,"<>
        " and the integration process will begin immediately."];
    Return[{"Integrable", {}}]
  ]; (* end If variationalDerivativeResult *)

  If[variationalDerivativeResult =!= testBasis,
    printOUT["The zero-Euler operator applied to the expression has not",
        " produced zero for all dependent variables.  The expression is",
        " not integrable. The integration process will not be activated."];
    Clear[j, variationalDerivativeResult, testBasis];
    Return[{"Not Integrable", {}}]
  ]; (* end If variationalDerivativeResult *)

 ] (* end Module integrabilityTest *)

(* ##########      Function: homotopyFinalIntegration        ########## *)

(************************************************************************)
(* homotopyFinalIntegration[expression, noOfDependentVar, indepVarList, *)
(*     degCheck]                                                        *)
(* Purpose:  To integrate an expression involving partial derivatives   *)
(*           in 1D and to invert the divergence operator in 2D and 3D.  *)
(* Input:    An expression                                              *)
(*           The number of dependent variables                          *)
(*           A list of independent variables                            *)
(* Output:   The inverted expression.                                   *)
(* Created:  6 March, 2006 by DP                                        *)
(* Code is in File:  newhomotopyoper/hofinint.m                         *)
(* Last Modified: 7 June, 2009, 16:05 by DP at CSM                      *)
(************************************************************************)

homotopyFinalIntegration[expression_, noOfDependentVar_, indepVarList_,
    degCheck_Integer] :=
Module[{numIndVars = Length[indepVarList], zero, lambdaPAR, expression2,
    indVar, uReplaceuLambdaRule, integrandList, sumOfDepComponents, lambdaSum,
    lambdaResult, homotopyAtOne, homotopyAtZero, interimResult, constantsk,
    divInterimResult, termsWithConstants, termsWOConstants, coefSystem,
    coefSystemSoln, homotopyResult, checkResult, dd, m1, m2, m3, m4, m5,
    printHFI},

  If[debugHomotopyFinalIntegration, printHFI = Print, Clear[printHFI],
      Clear[printHFI]];
  printHFI["debug code: HFI, Function: homotopyFinalIntegration,"<>
      " File: hofinint.m"];

  printHFI["At HFI IN, the given expression is:"];
  printHFI[expression];
  printHFI["At HFI IN, the number of dependent variables is ",
      noOfDependentVar];
  printHFI["At HFI IN, the list of independent variables given is ",
      indepVarList];
  printHFI["At HFI IN, the degree check is ", degCheck];

  (* If the expression is zero, return 0 vector.                        *)
  zero = Table[0, {i1, 1, numIndVars}];
  printHFI["At HFI 1, zero = ", zero];
  If[expression === 0, Return[zero]];

  (* Check to see if lambda is a parameter in the expression.           *)
  (* If so, lambda to another parameter, lambdaPAR.                     *)
  expression2 = If[MemberQ[expression, lambda, {0, Infinity}],
      expression /. lambda -> lambdaPAR, expression, expression];
  printHFI["At HFI 2, the expression after a check for lambdas:"];
  printHFI[expression2];


  indVar[1] = Part[indepVarList, 1];
  If[numIndVars > 1, indVar[2] = Part[indepVarList, 2]];
  If[numIndVars > 2, indVar[3] = Part[indepVarList, 3]];

  (* The uReplaceuLambda rule takes u from an expression and replaces   *)
  (* it with lambda*u.                                                  *)
  uReplaceuLambdaRule = {u[m1_][m2__] -> lambda*u[m1][m2],
      Derivative[m3__][u[m4_]][m5__] -> lambda*Derivative[m3][u[m4]][m5]};

  If[degCheck =!= 0,
    (* Use the integrand functions to determine the integrands for the  *)
    (* homotopy operator.                                               *)
    integrandList = Switch[numIndVars,
      1, Table[{integrandLeftDerList1D[expression2, j1, indepVarList]},
          {j1, 1, noOfDependentVar}],
      2, Table[integrandLeftDerList2D[expression2, j1, indepVarList],
          {j1, 1, noOfDependentVar}],
      3, Table[integrandLeftDerList3D[expression2, j1, indepVarList],
          {j1, 1, noOfDependentVar}]];
    printHFI["At HFI 3, a list of pieces produced by the integrand operators"<>
        " is given as follows.  The innermost nested list gives the"<>
        " integrand pieces for each dependent variable.  The outermost"<>
        " list{s} are groupings over each independent variable."];
    printHFI[integrandList];

    (* Add the terms produced by the integrand operator produced by     *)
    (* each dependent variable.                                         *)
    sumOfDepComponents = Apply[Plus, integrandList];
    printHFI["At HFI 4, the previous list has been refined by adding"<>
        " all of the dependent parts together.  The result consists of one"<>
        " expression for each independent variable."];
    printHFI[sumOfDepComponents];

    (* Apply the lambda replace rule.                                   *)
    lambdaSum = sumOfDepComponents /. uReplaceuLambdaRule;
    printHFI["At HFI 5, the result when u is replaced with u*lambda:"];
    printHFI[lambdaSum];

    (* Divide all terms by lambda and integrate with respect to lambda. *)
    lambdaSum = Expand[lambdaSum/lambda];
    printHFI["At HFI 6, the result after all terms have been divided by"<>
        " lambda:"];
    printHFI[lambdaSum];

    lambdaResult = Map[Integrate[#, lambda] &, lambdaSum, {1}];
    lambdaResult = Factor[Expand[lambdaResult]];
    printHFI["At HFI 7, the result after integrating with respect to lambda:"];
    printHFI[lambdaResult],

  (* else *)
    If[numIndVars === 1,
      lambdaResult = {Integrate[expression, indVar[1]]};
      printOUT["Mathematica's Integrate function has been called"<>
          " because a zero degree term existed in the expression to be"<>
          " integrated."];
      Return[lambdaResult],
    (* else *)
      printOUT["The homotopy operator is unable to invert the divergence"<>
          " on the given expression because at least one term in the"<>
          " expression has an overall degree of zero."];
      Return[{}]
    ]; (* end If numIndVars *)
  ]; (* end If degCheck *)

  If[MemberQ[lambdaResult, _Integrate, {0, Infinity}],
    printOUT["The homotopy operator was unable to perform the auxillary"<>
        " integration required in the final step because the function is"<>
        " to complicated to integrate.  It is unable to provide a result."];
    lambdaResult = {}
  ]; (* end If MemberQ *)

  homotopyAtOne = If[MemberQ[lambdaResult, Sin[_] | Cos[_] | Tan[_] | Cot[_] |
      Sec[_] | Csc[_] | Sinh | Cosh[_] | Tanh[_] | Coth[_] | Sech[_] | Csc[_],
      {0, Infinity}] === {},
    Expand[TrigExpand[lambdaResult /. {lambda -> 1}]],
    If[MemberQ[lambdaResult, Log[_], {0, Infinity}],
      Expand[PowerExpand[lambdaResult /. {lambda -> 1}]],
      Expand[lambdaResult /. {lambda -> 1}]]];
  printHFI["At HFI 9, the result from the homotopy operator after"<>
      " lambda -> 1:"];
  printHFI[homotopyAtOne];

  (* Check the result after lambda -> 1 by comparing its divergence     *)
  (* with the given expression.  If they match, return and finish.      *)
  checkResult = divergenceCheck[expression2, homotopyAtOne, indepVarList];
  printHFI["At HFI 10, a check on the integration result over lambda when"
      " lambda -> 1:"];
  printHFI[checkResult];
  If[checkResult === 0,
    (* Clear all variables not being passed back.                       *)
    Clear[numIndVars, zero, expression2, indVar, uReplaceuLambdaRule, lambdaSum,
        integrandList, sumOfDepComponents, lambdaResult, checkResult, printHFI];
    Return[homotopyAtOne /. lambdaPAR -> lambda]
  ]; (* end If checkResult] *)

  Off[Power::infy];
  Off[\[Infinity]::indet];

  homotopyAtZero = If[MemberQ[lambdaResult, lambda, {0, Infinity}],
       Expand[lambdaResult /. lambda -> 0], zero];
  printHFI["At HFI 11, the integration result over lambda when lambda -> 0:"];
  printHFI[homotopyAtZero];

  (* Mathematica may produce "constants" in terms of the dependent      *)
  (* variables when it integrates the integrand expression over lambda. *)
  (* These extra terms must be removed before reverting from lambda     *)
  (* space back to the independent variable space.                      *)

  (* First adjust the expression for any lambdas in the denominator.    *)
  choices = ComplexInfinity | Indeterminate | Infinity;
  If[MemberQ[homotopyAtZero, choices, {0, Infinity}],
    numHomotopyAtZero = Expand[Map[Numerator, lambdaResult]];
    printHFI["At HFI 12, the numerators of the result after integration"<>
        " over lambda:"];
    printHFI[numHomotopyAtZero];

    headNumParts = Map[Head, numHomotopyAtZero];
    printHFI["At HFI 13, the heads of the components of the result after"<>
        " integration:"];
    printHFI[headNumParts];

    numHomotopyAtZero = Map[Level[#, {1}] &, numHomotopyAtZero];
    printHFI["At HFI 14, the numerators as a list of terms:"];
    printHFI[numHomotopyAtZero];

    denHomotopyAtZero = Map[Denominator, lambdaResult];
    printHFI["At HFI 15, a list of denominators of the result after integration"<>
        " over lambda:"];
    printHFI[denHomotopyAtZero];

    splitHomotopyAtZero = Thread[tt[numHomotopyAtZero, 1/denHomotopyAtZero]] /.
        tt -> Times;
    printHFI["At HFI 16, a list of terms of the result after integration"<>
        " over lambda:"];
    printHFI[splitHomotopyAtZero];

    splitHomotopyAtZero = splitHomotopyAtZero /. lambda -> 0;
    printHFI["At HFI 17, the list of terms of the result after integration"<>
        " over lambda after lambda has been replaced with zero:"];
    printHFI[splitHomotopyAtZero];

    splitHomotopyAtZero = splitHomotopyAtZero /. {ComplexInfinity -> 0,
        Indeterminate -> 0};
    printHFI["At HFI 18, the list of terms of the result after integration"<>
        " over lambda after ComplexInfinity has been removed:"];
    printHFI[splitHomotopyAtZero];

    splitHomotopyAtZero = splitHomotopyAtZero /.
        Times[n1_, __] /; n1 === Infinity || n1 === -Infinity -> 0;
    printHFI["At HFI 19, the list of terms of the result after integration"<>
        " over lambda after +/-Infinity has been removed:"];
    printHFI[splitHomotopyAtZero];

    homotopyAtZero = Thread[app[headNumParts, splitHomotopyAtZero]] /.
        app -> Apply;
    printHFI["At HFI 20, the reconstructed result for lambda = 0:"];
    printHFI[homotopyAtZero]
  ]; (* end If MemberQ *)

  (* Remove numerical constants.                                         *)
  homotopyAtZero = If[MemberQ[homotopyAtZero, u[_][__] |
     Derivative[__][u[_]][__], {0, Infinity}], homotopyAtZero, zero];
  printHFI["At HFI 22, the integration result over lambda when lambda -> 0"<>
      " is changed to zero if the result contains only numerical constants:"];
  printHFI[homotopyAtZero];

  (* Second, check the expression with lambda -> 1 minus the expression *)
  (* with lambda -> 0.  If the divergence on the result matches the     *)
  interimResult = Expand[PowerExpand[Factor[homotopyAtOne - homotopyAtZero]]];
  printHFI["At HFI 23, the integration result with lambda -> 1 minus the"<>
      " integration result with lambda -> 0 before the shift is undone:"];
  printHFI[interimResult];

  checkResult = divergenceCheck[expression2, interimResult, indepVarList];
  printHFI["At HFI 24, a check on the integration result on the difference"<>
      " where lambda -> 1, then lambda -> 0 have been applied:"];
  printHFI[checkResult];
  If[checkResult === 0,
    On[Power::infy];
    On[\[Infinity]::indet];

    (* Clear all variables not being passed back.                       *)
    Clear[numIndVars, zero, expression2, indVar, uReplaceuLambdaRule, lambdaSum,
        integrandList, sumOfDepComponents, lambdaResult, homotopyAtOne,
        homotopyAtZero, checkResult, printHFI];

    Return[interimResult /. lambdaPAR -> lambda]
  ]; (* end If checkResult *)

  (* Third, it may be necessary to evaluate terms produced when         *)
  (* lambda -> 0.  Not all terms in this case can removed, so           *)
  (* comparisons must be made with the given expression.                *)
  If[homotopyAtZero =!= zero,
    homotopyAtZero = applyConstantsToVector[homotopyAtZero, numIndVars];
    constantsk = Part[homotopyAtZero, 4];
    homotopyAtZero = If[numIndVars === 1, Part[homotopyAtZero, 1],
        Table[Part[homotopyAtZero, j], {j, 1, numIndVars}]];
    printHFI["At HFI 25, zero terms with constants:"];
    printHFI[homotopyAtZero];

    interimResult = Expand[homotopyAtOne - homotopyAtZero];
    printHFI["At HFI 26, zero terms with constants subtracted from the"<>
        " result where lambda -> 1:"];
    printHFI[interimResult];

    divInterimResult = If[numIndVars === 1,
        D[Part[interimResult, 1], indVar[1]],
        If[numIndVars === 2,
            divergence[Flatten[{interimResult, 0}], indVar[1], indVar[2], dd],
        divergence[interimResult, indVar[1], indVar[2], indVar[3]]]];
    printHFI["At HFI 27, the revised homotopy result with constants on"<>
        " zero terms after the divergence has been applied:"];
    printHFI[divInterimResult];
    (* Compare the derivative/divergence of the result with the given   *)
    (* expression.                                                      *)
    divInterimResult = Factor[divInterimResult - expression];
    printHFI["At HFI 28, the difference between the revised homotopy"<>
        " result with constants and the original expression:"];
    printHFI[divInterimResult];

    divInterimResult = Expand[Numerator[divInterimResult]];
    printHFI["At HFI 29, the numerator of the difference:"];
    printHFI[divInterimResult];

    termsWithConstants = Apply[Plus,
        Cases[divInterimResult, Times[k[_], ___], {0, Infinity}]];
    printHFI["At HFI 30, the terms in the difference containing k[i]:"];
    printHFI[termsWithConstants];

    termsWOConstants = -Complement[divInterimResult, termsWithConstants];
    printHFI["At HFI 31, the terms in the difference without k[i]:"];
    printHFI[termsWOConstants];

    coefSystem = makeCoefficientEquations[termsWithConstants,
        constantsk, termsWOConstants, indepVarList, "caseTWO"];
    printHFI["At HFI 32, a system of equations formed from the coefficients:"];
    printHFI[coefSystem];

    coefSystemSoln = Flatten[Solve[coefSystem, constantsk]];
    printHFI["At HFI 33, the solution to the system of equations formed"<>
        " from the coefficients:"];
    printHFI[coefSystemSoln];

    homotopyResult = interimResult /. coefSystemSoln /. k[_] -> 0;
    homotopyResult = Factor[homotopyResult];
    printHFI["At HFI 34, the revised homotopy result (after removing"<>
        " constants left from Mathematica's integration over lambda):"];
    printHFI[homotopyResult];
  ]; (* end If homotopyAtZero *)

  On[Power::infy];
  On[\[Infinity]::indet];

  (* Clear all variables not being passed back.                         *)
  Clear[numIndVars, zero, expression2, indVar, uReplaceuLambdaRule, lambdaSum,
      integrandList, sumOfDepComponents, lambdaResult, homotopyAtOne,
      homotopyAtZero, interimResult, constantsk, divInterimResult, printHFI,
      termsWithConstants, termsWOConstants, coefSystem, coefSystemSoln];

  Return[homotopyResult /. lambdaPAR -> lambda]
]; (* end Module homotopyFinalIntegration *)

(* ##########        Function: applyConstantsToVector        ########## *)

(************************************************************************)
(* applyConstantsToVector[vector, lengthIndVar]                         *)
(* Purpose:  To apply constants k[i] through the components of a vector *)
(*           so that the indicies run from 1 to n through all           *)
(*           components.                                                *)
(* Input:    An expression in rational form                             *)
(*           The number of independent variables                        *)
(* Output:   The numerator of the expression                            *)
(* Created:  3 September, 2007 by DP                                    *)
(* Code is in File:  newhomotopyoper/hoaconve.m                         *)
(* Last Modified: 10 March, 2008, 14:56 by DP at CSM                    *)
(************************************************************************)

applyConstantsToVector[vector_, lengthIndVar_] :=
Module[{i1 = 1, lengthComp, homotopyResultList = {}, unknownConstants,
    homotopyResultPart, termsWithConstants, expnWithConstants, printACTV},

  If[debugApplyConstantsToVector, printACTV = Print, Clear[printACTV],
      Clear[printACTV]];
  printACTV["debug code: ACTV, Function: applyConstantsToVector,"<>
      " File: hoaconve.m"];

  printACTV["At ACTV IN, the vector given is:"];
  printACTV[vector];
  printACTV["At ACTV IN, the number of independent variables is ",
      lengthIndVar];

  (* Find the length of each component.                                 *)
  lengthComp[1] = If[Head[Part[vector, 1]] === Plus,
      Length[Part[vector, 1]], 1];
  lengthComp[2] = If[lengthIndVar > 1,
      If[Head[Part[vector, 2]] === Plus, Length[Part[vector, 2]], 1], 0];
  lengthComp[3] = If[lengthIndVar === 3,
      If[Head[Part[vector, 3]] === Plus, Length[Part[vector, 3]], 1], 0];
  printACTV["At ACTV 1, component 1 has length ", lengthComp[1], " "<>
      "component 2 has length ", lengthComp[2], ", and component 3 has"<>
      " length ", lengthComp[3]];

  (* Write all terms into a single list.                                *)
  While[i1 <= lengthIndVar,
    homotopyResultPart = If[Head[Part[vector, i1]] === Plus,
        Apply[List, Part[vector, i1]],
        Part[vector, i1]];
    printACTV["At ACTV  2, part ", i1, " of the homotopy result with "<>
        "", Length[homotopyResultPart], " terms has been converted to"<>
        " list form:"];
    printACTV[homotopyResultPart];
    homotopyResultList =
      Flatten[Append[homotopyResultList, homotopyResultPart]];
    printACTV["At ACTV 3, a full list of terms up to part ", i1,
      " of the homotopy result:"];
    printACTV[homotopyResultList];
    i1++
  ]; (* end While *)

  (* Set the list of unknown constants and match the constants with the  *)
  (* terms in the homotopy term list and with the ratings for each term. *)
  unknownConstants = Table[k[i], {i, 1, Length[homotopyResultList]}];

  (* Attach constants to the terms produced by the homotopy operator.    *)
  termsWithConstants = Thread[Times[homotopyResultList, unknownConstants]];
  printACTV["At ACTV 4, homotopy terms with their constants attached:"];
  printACTV[termsWithConstants];

  (* Remake the homotopy components from the list of homotopy terms with *)
  (* constants so that a divergence can be found.                        *)
  expnWithConstants[1] =
      Apply[Plus , Take[termsWithConstants, lengthComp[1]]];
  expnWithConstants[2] = If[lengthIndVar > 1, Apply[Plus,
      Take[termsWithConstants, {lengthComp[1] + 1,
      lengthComp[1] + lengthComp[2]}]], {}, {}];
  expnWithConstants[3] = If[lengthIndVar === 3, Apply[Plus,
      Take[termsWithConstants, {lengthComp[1] + lengthComp[2] + 1,
      Length[termsWithConstants]}]], {}, {}];
  printACTV["At ACTV 5, the homotopy result with constants is:"];
  printACTV[Flatten[{expnWithConstants[1], expnWithConstants[2],
      expnWithConstants[3]}]];

  Clear[i1, lengthComp, homotopyResultList, homotopyResultPart,
      termsWithConstants, printACTV];

  Return[{expnWithConstants[1], expnWithConstants[2],
          expnWithConstants[3], unknownConstants}]
]; (* end Module applyConstantsToVector *)

(* ##########     Function: variationalDerivativeMultiD      ########## *)

(************************************************************************)
(* variationalDerivativeMultiD[expression, dependentVar, indVarList]    *)
(* Purpose:  To find the variational derivative of the given expression *)
(*           in order to determine if the expression in integrable.     *)
(* Input:    An expression                                              *)
(*           A number for the dependent variable                        *)
(*           A list of independent variables                            *)
(* Output:   The variational derivative (result from applying the Euler *)
(*               operator) of an expression over a single dependent     *)
(*               variable                                               *)
(* Created:  23 May, 2006 by DP                                         *)
(* Code is in File:  conservationlaws/hovarder.m                        *)
(* Last Modified: 8 May, 2008, 10:47 by DP at CSM                       *)
(************************************************************************)

variationalDerivativeMultiD[expression_, dependentVar_, indVarList_] :=
Module[{lengthIndVars = Length[indVarList], indVar1 = Part[indVarList, 1],
    indVar2, indVar3, fullIndVarList, n1, n2, lengthFullIndVarList,
    termsWithDerList, depVarList, combinedList, higherOrderList, orderList,
    i, j, partialWRTpartialList, derivativeList, variationalDerivativeList,
    dummyderiv, variationalDerivative, printVDMD},

  If[debugVariationalDerivative, printVDMD = Print, Clear[printVDMD],
      Clear[printVDMD]];

  printVDMD["debug code: VDMD, Function: variationalDerivativeMultiD,"<>
      " File: variationalDerivativeMultiD.m"];

  printVDMD["At VDMD IN, the expression given:"];
  printVDMD[expression];
  printVDMD["At VDMD IN, the current dependent variable is ", dependentVar];
  printVDMD["At VDMD IN, the independent variables are ", indVarList];

  (* Assign a standard form to the independent variables.               *)
  If[lengthIndVars > 1, indVar2 = Part[indVarList, 2]];
  If[lengthIndVars > 2, indVar3 = Part[indVarList, 3]];

  fullIndVarList = Union[Cases[expression, Derivative[__][u[_]][__] | u[_][__],
      {0, Infinity}] /. {u[_][n1__] -> {n1}, Derivative[__][_][n2__] -> {n2}}];
  fullIndVarList = Flatten[fullIndVarList];
  printVDMD["At VDMD 1, the extended list of independent variables:"];
  printVDMD[fullIndVarList];

  lengthFullIndVarList = Length[fullIndVarList];

  (* Create a list of patial derivatives existing in the expression.    *)
  termsWithDerList = Cases[expression, Derivative[__][u[dependentVar]][__],
      {0, Infinity}];
  printVDMD["At VDMD 2, all derivatives in the expression are given in"<>
      " the following list:"];
  printVDMD[termsWithDerList];

  (* Check for terms that have no derivatives.                          *)
  depVarList = Cases[expression, u[dependentVar][__], {0, Infinity}];
  printVDMD["At VDMD 3, all terms in the expression without derivatives are"<>
      " given in the following list:"];
  printVDMD[depVarList];

  (* Put depVarList and derivative lists together. *)
  combinedList = Union[depVarList, termsWithDerList];
  printVDMD["At VDMD 4, derivative terms and non derivative terms"<>
      " contained in the given expression are shown in the following list:"];
  printVDMD[combinedList];

  (* Create a list of the orders of all derivatives that exist in the   *)
  (* expression.                                                        *)
  higherOrderList = Map[Head, Map[Head, termsWithDerList]] /.
      Derivative -> List;
  printVDMD["At VDMD 5, a list of orders for derivative terms only:"];
  printVDMD[higherOrderList];

  (* Include order if terms without derivatives. *)
  orderList = If[depVarList =!= {},
      Union[{Table[0, {i, 1, lengthFullIndVarList}]},
      higherOrderList], Union[higherOrderList], Union[higherOrderList]];
  printVDMD["At VDMD 6, a list of orders for all terms is:"];
  printVDMD[orderList];

  If[lengthIndVars < lengthFullIndVarList,
    orderList = Map[Extract[#,
      Flatten[Map[Position[fullIndVarList, #] &, indVarList], 1]] &, orderList];
  ]; (* end If lengthIndVars *)
  printVDMD["At VDMD 7, after extracting orders according to the independent"<>
      "variable list:"];
  printVDMD[orderList];

  (* Create a list of the partial derivatives of the expression with    *)
  (* respect to the partial derivative list created above. *)
  partialWRTpartialList = Map[D[expression, #] &, combinedList];
  printVDMD["At VDMD 8, the partial derivatives of the expression with"<>
      " respect to the terms given in the term list above are given in"<>
      " the following list:"];
  printVDMD[partialWRTpartialList];

  derivativeList = Table[{Part[{indVar1, indVar2, indVar3}, i],
      Part[orderList, j, i]},
      {j, 1, Length[orderList]}, {i, 1, lengthIndVars}];
  printVDMD["At VDMD 9, the number of total derivatives for each"<>
      " independent variable to be taken:"];
  printVDMD[derivativeList];

  (* Compute a list of terms that will add together to form the         *)
  (* variational derivative by taking a series of derivatives the       *)
  (*  previous list.                                                    *)
  variationalDerivativeList = Map[(-1)^Apply[Plus, #] &, orderList]*
      ReleaseHold[Thread[dummyderiv[partialWRTpartialList,
      Map[Hold[Apply[Sequence, #]] &, derivativeList]]]] /. dummyderiv -> D;
  printVDMD["At VDMD 10, after all derivatives have completed, the list of"<>
      " terms becomes:"];
  printVDMD[variationalDerivativeList];

  (* Add the terms in the variational derivative list.                  *)
  variationalDerivative = If[Cases[variationalDerivativeList,
      _?(Head[#] === Sin || Head[#] === Cos || Head[#] === Tan ||
      Head[#] === Sinh || Head[#] === Cosh || Head[#] === Tanh &),
      {0, Infinity}] =!= {},
      TrigExpand[Apply[Plus, variationalDerivativeList]],
      Expand[Apply[Plus, variationalDerivativeList]],
      variationalDerivativeList];
  variationalDerivative = If[variationalDerivative =!= 0,
      Factor[variationalDerivative], variationalDerivative,
      variationalDerivative];
  printVDMD["At VDMD 11, the variational derivative (result from"<>
      " applying the zero Euler operator) for u[", dependentVar, "] is:"];
  printVDMD[variationalDerivative];

  (* Clear all local variables not being returned.                      *)
  Clear[lengthIndVars, indVar1, indVar2, indVar3, fullIndVarList, n1, n2,
      lengthFullIndVarList, termsWithDerList, depVarList, combinedList,
      higherOrderList, orderList, i, j, partialWRTpartialList, derivativeList,
      variationalDerivativeList, dummyderiv, printVDMD];

  Return[variationalDerivative]
]; (* end Module variationalDerivativeMultiD *)

(* ##########              Function: divergence              ########## *)

divergence[expr_List, iv1_, iv2_:0, iv3_:0] :=
    D[Part[expr, 1], iv1] + If[iv2 === 0, 0, D[Part[expr, 2], iv2]] +
    If[iv3 === 0, 0, D[Part[expr, 3], iv3]];

(* ##########           Function: divergenceCheck            ########## *)

(************************************************************************)
(* divergenceCheck[expression, homotopyOpResult, indepVarList]          *)
(* Purpose:  To take the derivative of divergence on the result         *)
(*           provided by the homotopy operator and check to see         *)
(*           if it is the same as the given expression.                 *)
(* Input:    The original given expression                              *)
(*           The calculated result from the homotopy operator           *)
(*           A list of independent variables                            *)
(* Output:   Yes or No.                                                 *)
(* Created:  30 August, 2007 by DP                                      *)
(* Code is in File:  newhomotopyoper/hodivchk.m                         *)
(* Last Modified: 20 March, 2008, 11:30 by DP at CSM                    *)
(************************************************************************)

divergenceCheck[expression_, homotopyOpResult_, indepVarList_] :=
Module[{numIndVars = Length[indepVarList], newDiv, divCheck},

  If[debugDivergenceCheck, printDC = Print, Clear[printDC], Clear[printDC]];
  printDC["debug code: DC, Function: divergenceCheck,"<>
      " File: hodivdhk.m"];

  printDC["At DC IN, the given expression is:"];
  printDC[expression];
  printDC["At DC IN, the homotopy result is:"];
  printDC[homotopyOpResult];
  printDC["At DC IN, the list of independent variables given is ",
      indepVarList];

    newDiv = divergence[homotopyOpResult, Apply[Sequence, indepVarList]];
    printDC["At DC 1, the "<>If[numIndVars === 1, "derivative", "divergence"]<>
        " of the result produced by the homotopy operator:"];
    printDC[newDiv];

    printDC["Checking the "<>If[numIndVars === 1, "derivative", "divergence"]<>
        " against the given expression:"];
    divCheck = Expand[expression - newDiv];
    printDC["At DC 2, applying Expand to the difference:"];
    printDC[divCheck];

    If[divCheck =!= 0,
      divCheck = Factor[divCheck];
      printDC["At DC 3, applying Factor to the difference:"];
      printDC[divCheck]
    ]; (* end If divCheck *)

    If[divCheck =!= 0,
      divCheck = Expand[TrigToExp[divCheck]];
      printDC["At DC 4, applying TrigToExp to the difference:"];
      printDC[divCheck]
    ]; (* end If divCheck *)

    If[divCheck =!= 0,
      divCheck = FullSimplify[divCheck, TimeConstraint -> 100];
      printDC["At DC 5, applying FullSimplify to the difference:"];
      printDC[divCheck]
    ]; (* end If divCheck *)

  Clear[numIndVars, newDiv];

  If[divCheck === 0,
    Return[0],
    Return[1]]
]; (* end Module divergenceCheck *)

(* ##########           Function: degreeOperator             ########## *)

(************************************************************************)
(* degreeOperator[expression, dependentVar, IndepVars]                  *)
(* Purpose:  To determine the overall degree of a differential function *)
(* Input:    An expression                                              *)
(*           The number of dependent variables                          *)
(*           A list of independent variables                            *)
(* Output:   A revised expression with information about its degree     *)
(*           contained in its coefficient                               *)
(* Created:  11 June, 2007 by DP                                        *)
(* Code is in File:  newhomotopyoper/hodegopr.m                         *)
(* Last Modified: 24 March, 2008 17:28 by DP at CSM                     *)
(************************************************************************)

degreeOperator[term_, numDependentVar_Integer, IndepVars_List] :=
Module[{numIndVar, termsWithDerList, depVarList, combinedList,
    partialWRTpartialList, counter = 1, finalDegree = {}},

  printDO["At DO IN 2, the term given is:"];
  printDO[term];
  printDO["At DO IN 2, the number of dependent variables is ",
      numDependentVar];
  printDO["At DO IN 2, the list of independent variables contains ",
      IndepVars];

  numIndVar = Length[IndepVars];

  (* Add results for all dependent variables.                           *)
  While[counter <= numDependentVar,

    (* Create a list of patial derivatives existing in the expression.  *)
    termsWithDerList = Cases[term,
        Derivative[__][u[counter]][__], {0, Infinity}];
    printDO["At DO 2, all derivatives in the expression are given in"<>
        " the following list:"];
    printDO[termsWithDerList];

    (* Check for terms that have no derivatives.                        *)
    depVarList = Cases[term, u[counter][__], {0, Infinity}];

    (* Put depVarList and derivative lists together. *)
    combinedList = Union[depVarList, termsWithDerList];
    printDO["At DO 3, derivative terms and non derivative terms"<>
        " contained in the given expression are shown in the following list:"];
    printDO[combinedList];

    (* Create a list of the partial derivatives of the expression with  *)
    (* respect to the partial derivative list created above. *)
    partialWRTpartialList = Map[D[term, #] &, combinedList];
    printDO["At DO 4, the partial derivatives of the expression with"<>
        " respect to the terms given in the term list above are given in"<>
        " the following list:"];
    printDO[partialWRTpartialList];

    (* Multiply the partial term to the partial derivative just found.  *)
    partialWRTpartialList = Thread[Times[partialWRTpartialList, combinedList]];
    partialWRTpartialList = Expand[partialWRTpartialList];
    printDO["At DO 5, the partial derivatives multiplied to the partial"<>
        " expression:"];
    printDO[partialWRTpartialList];

    finalDegree = Flatten[Append[finalDegree, partialWRTpartialList]];
    printDO["At DO 6, a list of all expressions computed so far:"];
    printDO[finalDegree];
    counter ++
  ]; (* end While *)

  finalDegree = Factor[Apply[Plus, finalDegree]];

  Clear[numIndVar, termsWithDerList, depVarList, combinedList,
    partialWRTpartialList, counter];

  Return[finalDegree]
]; (* end Module degreeOperator *)

degreeOperExpression[expression_, numDepVars_Integer, indVarsList_List] :=
Module[{expandedExpression, degreeOperResult},

  If[debugDegreeOperator, printDO = Print, Clear[printDO], Clear[printDO]];
  printDO["debug code: DO, Function: degreeOperator, File: hodegopr.m"];

  printDO["At DO IN 1, the given expression is:"];
  printDO[expression];
  printDO["At DO IN 1, the number of dependent variables is ", numDepVars];
  printDO["At DO IN 1, the list of independent variables contains ",
      indVarsList];

  expandedExpression = Expand[expression];

  expandedExpression = If[Head[expandedExpression] === Plus,
      Apply[List, expandedExpression], {expression}, {expression}];
  printDO["At DO 1, a list of terms in the expression:"];
  printDO[expandedExpression];

  degreeOperResult = Map[degreeOperator[#, numDepVars, indVarsList] &,
      expandedExpression];
  printDO["At DO 7, the expression returned by the degree operator:"];
  printDO[degreeOperResult];

  Return[{expandedExpression, degreeOperResult}]
]; (* end Module degreeOperExpression *)

(* ##########       Function: rationalExpressionCheck        ########## *)

(************************************************************************)
(* rationalExpressionCheck[expressionGiven, indepVars, numDepVars]      *)
(* Purpose:  To check an expression for terms that fall in the kernel   *)
(*           of the degree operator, then to separate those terms from  *)
(*           the rest of the expression.  The separate pieces will be   *)
(*           run through the homotopy operator independently            *)
(* Input:    An expression                                              *)
(*           A list of terms in the denominator                         *)
(* Output:   The combined result from the homotopy operator             *)
(* Created:  27 March, 2008 by DP                                       *)
(* Code is in File:  newhomotopyoper/horatchk.m                         *)
(* Last Modified: 1 April, 2008, 18:03 by DP at CSM                     *)
(************************************************************************)

rationalExpressionCheck[expressionGiven_, indepVars_, numDepVars_] :=
Module[{expnDenom, revisedExpn, degree, zeroDegreeTerms, shiftedTerms,
    afterHomotopy, degreeChk, printREC},

  If[debugRationalExpressionCheck, printREC = Print, Clear[printREC],
      Clear[printREC]];
  printREC["debug code: REC, Function: rationalExpressionCheck,"<>
      " File: horatchk.m"];

  printREC["At REC IN, the given expression is:"];
  printREC[expressionGiven];
  printREC["At REC IN, the list of independent variables is ", indepVars];
  printREC["At REC IN, the number of dependent variables is ", numDepVars];

  (* Check the expression for an overall degree of zero.  If the degree *)
  (* is zero, the homotopy operator will not work until changes are     *)
  (* made.                                                              *)
  expnDenom = MemberQ[expressionGiven, Power[n1__, n2_] /;
      MemberQ[n1, u[_][__] | Derivative[__][u[_]][__], {0, Infinity}] &&
      n2 < 0, {0, Infinity}];
  printREC["At REC 1, True indicates that the expression is rational: ",
      expnDenom];

  If[expnDenom,
    {revisedExpn, degree} = degreeOperExpression[expressionGiven,
        numDepVars, indepVars];
    printREC["At REC 2, the results after applying the degree operator:"];
    printREC[degree];
    printREC["At REC 3, if terms in the expression have denominators, the"<>
        " result here is the expanded expression in a list.  If there are no"<>
        " denominators, the original expression is given."];
    printREC[revisedExpn];

    (* If the expression has a term with a degree of zero, shift one of *)
    (* the terms in the denominator of the degree zero term.            *)
    If[MemberQ[degree, 0],
      zeroDegreeTerms = Extract[revisedExpn, Position[degree, 0, {0,1}]];
      printREC["At REC 4, the terms in the expression that have degree 0:"];
      printREC[zeroDegreeTerms];

      shiftedTerms = shiftDenominatorTerm[Apply[Plus, zeroDegreeTerms],
          indepVars, numDepVars];
      printREC["At REC 5, an attempt to apply a shift to one term in the"<>
          " denominator:"];
      printREC[shiftedTerms];

      revisedExpn = {Apply[Plus, Complement[revisedExpn, zeroDegreeTerms]],
          shiftedTerms};
      printREC["At REC 6,the original expression has been split into two"<>
          " pieces, a piece not in the kernel of the degree operator"<>
          " unchanged and a piece in the kernel of the degree operator"<>
          " with denominators shifted:"];
      printREC[revisedExpn];

      (* Recheck the degree before moving on.                           *)
      degreeChk = degreeOperExpression[Part[revisedExpn, 2], numDepVars,
          indepVars];
      printREC["At REC 7, the results after applying the degree operator"<>
          " to the part in the kernel of the degree operator:"];
      printREC[Part[degreeChk, 2]];

      (* If degree sends 0 to homotopyFinalIntegration, Mathematica's   *)
      (* Integrate will be called for 1D functions.                     *)
      degreeChk = If[MemberQ[Part[degreeChk, 2], 0], 0, 1];
      printREC["At REC 8, the degree check now shows ", degreeChk, ".  If"<>
          " the degree check is 1, the homotopy operator will work."];

      revisedExpn = If[degreeChk === 0, {expressionGiven}, revisedExpn];
      printREC["At REC 9, the given expression is shown if the degree"<>
          " check shows 0; otherwise the expression in REC 6 is shown:"];
      printREC[revisedExpn];

      (* Run the homotopy operator independently on the terms not in    *)
      (* the kernel of the degree operator, the on the terms in the     *)
      (* kernel of the degree operator.                                 *)
      afterHomotopy = Map[homotopyFinalIntegration[
          #, numDepVars, indepVars, degreeChk] &, revisedExpn];
      printREC["At REC 10, the result returned from the homotopy operator:"];
      printREC[afterHomotopy];

      (* Undo any shifts applied to terms in the kernel of the degree   *)
      (* operator.                                                      *)
      afterHomotopy = afterHomotopy /. Log[shift] -> 0 /. shift -> 0;
      printREC["At REC 11, the results returned from the homotopy operator"<>
          " after all shifts have been removed:"];
      printREC[afterHomotopy];

      afterHomotopy = MapThread[Plus, afterHomotopy, 1];
      printREC["At REC 12, the combined results returned from the homotopy"<>
          " operator:"];
      printREC[afterHomotopy],

    (* else *)
      degreeChk = 1;
      afterHomotopy = homotopyFinalIntegration[expressionGiven, numDepVars,
          indepVars, degreeChk];
      printREC["At REC 13, the result returned from the homotopy operator:"];
      printREC[afterHomotopy]
    ], (* end If MemberQ[degree] *)

  (* else *)
    degreeChk = 1;
    afterHomotopy = homotopyFinalIntegration[expressionGiven, numDepVars,
        indepVars, degreeChk];
    printREC["At REC 14, the result returned from the homotopy operator:"];
    printREC[afterHomotopy]
  ]; (* end If expnDenom *)

  (* Clear all variables not being passed back.                         *)
  Clear[expnDenom, revisedExpn, degree, zeroDegreeTerms, shiftedTerms,
      degreeChk, printREC];

  Return[afterHomotopy]
] (* end Module rationalExpressionCheck *)

(* ##########      Function: shiftDenominatorTerm        ########## *)

(************************************************************************)
(* shiftDenominatorTerm[expressionGiven, indepVars, numDepVars]         *)
(* Purpose:  To shift a term in the denominator of a rational expression*)
(*           away from the origin.  This alleviates the homogenous      *)
(*           degree problem.                                            *)
(* Input:    An expression                                              *)
(*           A list of terms in the denominator                         *)
(* Output:   A list of degrees corresponding to each term in the        *)
(*               expression                                             *)
(* Created:  19 March, 2008 by DP                                       *)
(* Code is in File:  newhomotopyoper/hoshift.m                          *)
(* Last Modified: 1 April, 2008, 18:06 by DP at CSM                     *)
(************************************************************************)

shiftDenominatorTerm[expressionGiven_, indepVars_, numDepVars_] :=
Module[{expression, zero, denominatorList, denominatorTermList, orderList,
    commonTerms, varDerResult = 1, termsToShift, shiftedExpression, pls,
    shiftedTerms, printSHD},

  If[debugShiftDenominatorTerm, printSHD = Print, Clear[printSHD],
      Clear[printSHD]];
  printSHD["debug code: SHD, Function: shiftDenominatorTerm,"<>
      " File: hoshift.m"];

  printSHD["At SHD IN, the given expression is:"];
  printSHD[expressionGiven];
  printSHD["At SHD IN, the list of independent variables is ", indepVars];
  printSHD["At SHD IN, the number of dependent variables is ", numDepVars];

  (* Initializations *)
  Clear[shift];
  zero = Table[0, {i1, 1, numDepVars}];
  expression = Expand[expressionGiven];
  printSHD["At SHD 1, the given expression after Expand has been applied:"];
  printSHD[expression];

  (* Get the denominators of the terms with a degree of zero.           *)
  denominatorList = Map[Denominator, Apply[List, expression]];
  printSHD["At SHD 2, a list of all denominators in the expression:"];
  printSHD[denominatorList];

  denominatorTermList = Union[Map[Cases[#, u[_][__] |
      Derivative[__][u[_]][__], {0, Infinity}] &, denominatorList]];
  denominatorTermList = DeleteCases[denominatorTermList, {}];
  printSHD["At SHD 3, jet space terms found in the denominators of all"<>
      " degree zero terms:"];
  printSHD[denominatorTermList];

  (* Sort the terms in the denominator by order.                        *)
  orderList = denominatorTermList /. {u[_][__] -> 0,
      Derivative[n1__][u[_]][__] -> pls[n1]} /. pls -> Plus;
  printSHD["At SHD 4, the orders corresponding to the jet space terms in"<>
       " the denominator list:"];
  printSHD[orderList];

  denominatorTermList = Sort[Thread[List[orderList, denominatorTermList]]];
  printSHD["At SHD 5, the denominators paired with their order sorted in"<>
      " decending order:"];
  printSHD[denominatorTermList];

  denominatorTermList = Map[Part[#, 2] &, denominatorTermList];
  printSHD["At SHD 6, the denominator list sorted in by order:"];
  printSHD[denominatorTermList];

  commonTerms = Apply[Intersection, denominatorTermList];
  printSHD["At SHD 7, all denominators contain these terms:"];
  printSHD[commonTerms];

  denominatorTermList = DeleteCases[denominatorTermList,
      Apply[Alternatives, commonTerms], {0, 3}];
  printSHD["At SHD 8, list of terms in the denominator after all common"<>
      " terms have been removed:"];
  printSHD[denominatorTermList];

  While[commonTerms =!= {} && varDerResult =!= zero ,
    termsToShift = First[commonTerms];
    printSHD["At SHD 9, this term will be shifted:"];
    printSHD[termsToShift];

    shiftedTerms = Rule[termsToShift, termsToShift + shift];
    printSHD["At SHD 10, the rule for making the shift on the term:"];
    printSHD[shiftedTerms];

    shiftedExpression = expression /. shiftedTerms;
    printSHD["At SHD 11, the expanded given expression with the shift:"];
    printSHD[shiftedExpression];

    varDerResult = Factor[Table[variationalDerivativeMultiD[
        shiftedExpression, i1, indepVars], {i1, 1, numDepVars}]];
    printSHD["At SHD 12, the variational derivative applied to the"<>
        " shifted expression:"];
    printSHD[varDerResult];

    commonTerms = If[commonTerms =!= {},
        Drop[commonTerms, 1], commonTerms];
  ]; (* end While *)

  denominatorTermList = If[varDerResult === zero,
      denominatorTermList, Tuples[denominatorTermList]];
  printSHD["At SHD 13, all groups of denominator terms that can be"<>
      " shifted:"];
  printSHD[denominatorTermList];

  While[Flatten[denominatorTermList] =!={} && varDerResult =!= zero,
    termsToShift = First[denominatorTermList];
    printSHD["At SHD 14, the terms in the following list will be shifted:"];
    printSHD[termsToShift];

    shiftedTerms = Map[Rule[#, # + shift] &, termsToShift];
    printSHD["At SHD 15, the rules for making the shift on the terms:"];
    printSHD[shiftedTerms];

    shiftedExpression = expression /. shiftedTerms;
    printSHD["At SHD 16, the expanded given expression:"];
    printSHD[shiftedExpression];

    varDerResult = Factor[Expand[Table[variationalDerivativeMultiD[
        shiftedExpression, i1, indepVars], {i1, 1, numDepVars}]]];
    printSHD["At SHD 17, the variational derivative applied to the"<>
        " shifted expression:"];
    printSHD[varDerResult];

    denominatorTermList = If[denominatorTermList =!= {},
        Drop[denominatorTermList, 1], denominatorTermList];
  ]; (* end While *)

  shiftedExpression = If[varDerResult =!= zero,
      expression, shiftedExpression];

  (* Clear all variables not being passed back.                         *)
  Clear[expression, zero, denominatorList, denominatorTermList, orderList,
    commonTerms, varDerResult, termsToShift, shiftedTerms, printSHD];

  Return[shiftedExpression]
] (* end Module shiftDenominatorTerm *)

(* ##########        Function: integrandLeftDerList1D        ########## *)

(************************************************************************)
(* integrandLeftDerList1D[expression, dependentVar, indepVarList]       *)
(* Purpose:  To find the integrand for the 1-D homotopy operator        *)
(*           using the form where the derivatives of the dependent      *)
(*           varialble have been propagated outside the inner most sum  *)
(*           (after Kruskal et. al.) In this version, operators are     *)
(*           determined by manipulating  lists                          *)
(* Input:    An expression                                              *)
(*           A number for the dependent variable                        *)
(*           A list of independent variables                            *)
(* Output:   The result of applying the higher Euler operators to the   *)
(*           expression.                                                *)
(* Created:  22 February, 2006 by DP                                    *)
(* Code is in File:  newhomotopyoper/hoingr1d.m                         *)
(* Last Modified: 9 April, 2008, 12::40 by DP at CSM                    *)
(************************************************************************)

integrandLeftDerList1D[expression_, dependentVar_, indepVarList_] := Module[
    {derivativeList, orderList, i, maxi, partialWRTpartialList, innerSumList,
    dummyderiv, innerSum, outerSumList, integrandResult, k,
    indVar1 = Part[indepVarList, 1], printIL1D},

  If[debugIntegrandLeftDerList, printIL1D = Print, Clear[printIL1D],
      Clear[printIL1D]];

  printIL1D["debug code: IL1D, Function: integrandLeftDerList1D,"<>
      " File: integrandLeftDerList1D.m"];

  printIL1D["Computing the integrand for the 1-D homotopy operator."];
  printIL1D["At IL1D IN, the expression given is:"];
  printIL1D[expression];
  printIL1D["At IL1D IN, the current dependent variable is"<>
      " u[", dependentVar, "]."];
  printIL1D["At IL1D IN, the independent variable is ", indepVarList];

  timeKeeper = TimeUsed[];

  (* Create a list of patial derivatives existing in the expression.    *)
  derivativeList = Union[Cases[expression,
      Derivative[___][u[dependentVar]][indVar1], {0, Infinity}]];
  printIL1D["At IL1D 1, derivatives contained in the given expression are"<>
      " shown in the following list:"];
  printIL1D[derivativeList];

  (* Create a list of the orders of all derivatives that exist in the   *)
  (* expression.                                                        *)
  orderList = Flatten[Map[Head, Map[Head, derivativeList]] /.
      Derivative -> List];
  printIL1D["At IL1D 2, a list of orders for all derivatives is:"];
  printIL1D[orderList];

  maxi = Max[orderList, 0];
  printIL1D["At IL1D 3, the order for the expression is ", maxi];

  (* Create a list of the partial derivatives of the expression with    *)
  (* respect to the partial derivative list created above.              *)
  partialWRTpartialList = Map[D[expression, #] &, derivativeList];
  printIL1D["At IL1D 4, the partial derivatives of the expression with"<>
      " respect to the terms given in the derivative list above are given"<>
      " in the following list:"];
  printIL1D[partialWRTpartialList];

  (* Create a list of terms for the inner sum of the formula using      *)
  (* the partial derivative list created in the last step.              *)
  innerSumList = Table[Map[(-1)^(# - i - 1) &,
      Select[orderList, # > i &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, _?(# > i &)]],
      Map[{indVar1, # - i - 1} &, Select[orderList, # > i &]]]]
      /. dummyderiv -> D, {i, 0, maxi - 1}];
  printIL1D["At IL1D 5, a list of inner sum terms for"<>
      " variable ", indVar1, " is:"];
  printIL1D[innerSumList];

  (* Add the terms in the sum list.                                     *)
  innerSum = Expand[Apply[Plus, innerSumList, 1]];
  printIL1D["At IL1D 6, after adding all inner sum terms for"<>
      " variable ", indVar1, ":"];
  printIL1D[innerSum];

  (* Multiply the sum by the derivatives of the dependent varialble     *)
  (* with respect to x according to the outer sum.  The result is a     *)
  (* list of terms.                                                     *)
  outerSumList = Expand[Table[Part[innerSum, i + 1]*
      Derivative[i][u[dependentVar]][indVar1], {i, 0, maxi - 1}]];
  printIL1D["At IL1D 7, a list of outer sum terms for"<>
      " variable ", indVar1, " is:"];
  printIL1D[outerSumList];

  (* Add the terms from the list in the last step to get the result of  *)
  (* applying the higher Euler operator.                                *)
  integrandResult = Expand[Apply[Plus, outerSumList]];
  printIL1D["At IL1D 8, the integrand for the 1-D homotopy operator is as"<>
      " follows:"];
  printIL1D[integrandResult];

  (* Clear all variables not being passed back.                         *)
  Clear[derivativeList, orderList, i, k, maxi, partialWRTpartialList,
      innerSumList, dummyderiv, innerSum, outerSumList, indVar1, printIL1D];

  Return[integrandResult]
] (* end Module integrandLeftDerList1D *)

(* ##########        Function: integrandLeftDerList2D        ########## *)

(************************************************************************)
(* integrandLeftDerList2D[expression, dependentVar, indepVarList]       *)
(* Purpose:  To find the integrand for the 2-D homotopy operator        *)
(*           using the form where the derivatives of the dependent      *)
(*           varialble have been propagated outside the inner most sum  *)
(*           (after Kruskal et. al.) In this version, operators are     *)
(*           determined by manipulating  lists                          *)
(* Input:    An expression                                              *)
(*           A number for the dependent variable                        *)
(*           A list of independent variables                            *)
(* Output:   The result of applying the higher Euler operators to the   *)
(*           expression.                                                *)
(* Created:  27 February, 2006 by DP                                    *)
(* Code is in File:  newhomotopyoper/hoingr2d.m                         *)
(* Last Modified: 9 April, 2008, 12:42 by DP at CSM                     *)
(************************************************************************)

integrandLeftDerList2D[expression_, dependentVar_, indepVarList_] :=
Module[{derivativeList, ix, iy, kx, ky, dummyderiv, orderList,
    maxix, maxiy, partialWRTpartialList, innerSumListx, innerSumListy,
    innerSumx, innerSumy, positionNonZeroTermsx, positionNonZeroTermsy,
    listWOzerosx, listWOzerosy, indVar1 = Part[indepVarList, 1],
    indVar2 = Part[indepVarList, 2], integrandResultx, integrandResulty,
    outerSumListx, outerSumListy, printIL2D},

  If[debugIntegrandLeftDerList, printIL2D = Print, Clear[printIL2D],
      Clear[printIL2D]];

  printIL2D["debug code: IL2D, Function: integrandLeftDerList2D,"<>
      " File: integrandLeftDerList2D.m"];

  printIL2D["Computing the integrand for the 2-D homotopy operator."];
  printIL2D["At IL2D IN, the expression given is:"];
  printIL2D[expression];
  printIL2D["At IL2D IN, the current dependent variable is"<>
      " u[", dependentVar, "]."];
  printIL2D["At IL2D IN, the independent variables are ", indepVarList];

  (* Create a list of patial derivatives existing in the expression.    *)
  derivativeList = Union[Cases[expression,
      Derivative[___][u[dependentVar]][indVar1, indVar2], {0, Infinity}]];
  printIL2D["At IL2D 1, the following list contains derivative terms from"<>
      " the given expression:"];
  printIL2D[derivativeList];

  (* Create lists of the orders of all derivatives that exist in the    *)
  (* expression. *)
  orderList = Map[Head, Map[Head, derivativeList]] /. Derivative -> List;
  printIL2D["At IL2D 2, a list of orders for the derivative terms shown"<>
      " above with orders for each variable listed in the form"<>
      " {", indVar1, ", ", indVar2, "} is:"];
  printIL2D[orderList];

  (* Determine the highest order derivative for each variable.          *)
  maxix = Max[Map[Part[#, 1] &, orderList], 0];
  maxiy = Max[Map[Part[#, 2] &, orderList], 0];
  printIL2D["At IL2D 3, the order for the expression in"<>
      " variable ", indVar1, " is ", maxix, " and the order for the"<>
      " expression in variable ", indVar2, " is ", maxiy, "."];

  (* Create lists for each variable of the partial derivatives of the   *)
  (* expression with respect to the partial derivative list created     *)
  (* above.                                                             *)
  partialWRTpartialList = Map[D[expression, #] &, derivativeList];
  printIL2D["At IL2D 4, the partial derivatives of the expression with"<>
      " respect to the terms given in the derivative list above are:"];
  printIL2D[partialWRTpartialList];

  (* Create lists of terms for the inner sum of the new formula using   *)
  (* the partial derivative list created in the last step.              *)
  innerSumListx = Table[Map[(-1)^(Part[#, 1] + Part[#, 2] - ix - iy - 1)*
      Binomial[Part[#, 1] + Part[#, 2] - ix - iy - 1, Part[#1, 1] - ix - 1]/
      Binomial[Part[#, 1] + Part[#, 2], Part[#, 1]] &,
      Select[Select[orderList, Part[#, 1] > ix &], Part[#, 2] >= iy &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, {_, _}?(Part[#, 1] > ix && Part[#, 2] >= iy &)]],
      Map[{indVar1, Part[#, 1] - ix - 1} &,
      Select[Select[orderList, Part[#, 1] > ix &], Part[#, 2] >= iy &]],
      Map[{indVar2, Part[#, 2] - iy} &,
      Select[Select[orderList, Part[#, 1] > ix &], Part[#, 2] >= iy &]]]]
      /. dummyderiv -> D, {iy, 0, maxiy}, {ix, 0, maxix - 1}];
  printIL2D["At IL2D 5x, a list of inner sum terms for"<>
      " variable ", indVar1, " is:"];
  printIL2D[innerSumListx];

  innerSumListy = Table[Map[(-1)^(Part[#, 1] + Part[#, 2] - ix - iy - 1)*
      Binomial[Part[#, 1] + Part[#, 2] - ix - iy - 1, Part[#, 2] - iy - 1]/
      Binomial[Part[#, 1] + Part[#, 2], Part[#, 2]] &,
      Select[Select[orderList, Part[#, 1] >= ix &], Part[#, 2] > iy &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, {_, _}?(Part[#, 1] >= ix && Part[#, 2] > iy &)]],
      Map[{indVar1, Part[#, 1] - ix} &,
      Select[Select[orderList, Part[#, 1] >= ix &], Part[#, 2] > iy &]],
      Map[{indVar2, Part[#, 2] - iy - 1} &,
      Select[Select[orderList, Part[#, 1] >= ix &], Part[#, 2] > iy &]]]]
      /. dummyderiv -> D, {iy, 0, maxiy - 1}, {ix, 0, maxix}];
  printIL2D["At IL2D 5y, a list of inner sum terms for"<>
      " variable ", indVar2, " is:"];
  printIL2D[innerSumListy];

  (* Add the terms in the sum lists to produce lists of inner sums      *)
  (* corresponding to each outer index for ix and iy. *)
  innerSumx = Expand[Apply[Plus, innerSumListx, {2, 2}]];
  printIL2D["At IL2D 6x, after adding all inner sum terms for"<>
      " variable ", indVar1, ":"];
  printIL2D[innerSumx];

  innerSumy = Expand[Apply[Plus, innerSumListy, {2, 2}]];
  printIL2D["At IL2D 6y, after adding all inner sum terms for"<>
      " variable ", indVar2, ":"];
  printIL2D[innerSumy];

  (* Remove all zeros from the lists of sums above and keep track of    *)
  (* the positions of the remaining terms.  The positions give the      *)
  (* outer index, ix and iy, for each term (in reverse order).          *)
  positionNonZeroTermsx = Position[innerSumx,
      _?(# =!= 0 && # =!= List &), {2, 2}];
  printIL2D["At IL2D 7x, the positions of all nonzero terms in the inner"<>
      " sum list for ", indVar1, " shifted by {1, 1, 1}:"];
  printIL2D[positionNonZeroTermsx];

  positionNonZeroTermsy = Position[innerSumy,
      _?(# =!= 0 && # =!= List &), {2, 2}];
  printIL2D["At IL2D 7y, the positions of all nonzero terms in the inner"<>
      " sum list for ", indVar2, " shifted by {1, 1, 1}:"];
  printIL2D[positionNonZeroTermsy];

  listWOzerosx = Flatten[DeleteCases[innerSumx, 0, 2]];
  printIL2D["At IL2D 8x, the inner sum list for ", indVar1, " after"<>
      " removing zero terms:"];
  printIL2D[listWOzerosx];

  listWOzerosy = Flatten[DeleteCases[innerSumy, 0, 2]];
  printIL2D["At IL2D 8y, the inner sum list for " indVar2, " after"<>
      " removing zero terms:"];
  printIL2D[listWOzerosx];

  (* Multiply the sum by the partial derivative of the dependent        *)
  (* variable with respect to the outer sum indices (ix and iy) and     *)
  (* constant determined by the outer sum indicies.                     *)
  outerSumListx = Thread[Times[listWOzerosx,
      Map[Binomial[Part[#, 2] + Part[#, 1] - 2, Part[#, 2] - 1]*
      Derivative[Part[#, 2] - 1, Part[#, 1] - 1][u[dependentVar]][indVar1, indVar2] &,
      positionNonZeroTermsx]]];
  printIL2D["At IL2D 9x, a list of outer sum terms for"<>
      " variable ", indVar1, " is:"];
  printIL2D[outerSumListx];

  outerSumListy = Thread[Times[listWOzerosy,
      Map[Binomial[Part[#, 2] + Part[#, 1] - 2, Part[#, 1] - 1]*
      Derivative[Part[#, 2] - 1, Part[#, 1] - 1][u[dependentVar]][indVar1, indVar2] &,
      positionNonZeroTermsy]]];
  printIL2D["At IL2D 9y, a list of outer sum terms for"<>
      " variable ",indVar2, " is:"];
  printIL2D[outerSumListy];

  (* Add the terms from the lists in the last step to get the result of *)
  (* applying the higher Euler operator for each variable.              *)
  integrandResultx = Expand[Apply[Plus, outerSumListx]];
  integrandResulty = Expand[Apply[Plus, outerSumListy]];
  printIL2D["At IL2D 10, the integrands for the 2-D homotopy operator for"<>
      " dependent variable u[", dependentVar, "] are as follows:"];
  printIL2D["For variable ", indVar1, ": ", integrandResultx];
  printIL2D["For variable ", indVar2, ": ", integrandResulty];

  (* Clear all variables not being passed back.                         *)
  Clear[derivativeList, ix, iy, kx, ky, dummyderiv, orderList,
    maxix, maxiy, partialWRTpartialList, innerSumListx, innerSumListy,
    innerSumx, innerSumy, positionNonZeroTermsx, positionNonZeroTermsy,
    listWOzerosx, listWOzerosy, indVar1, indVar2, outerSumListx,
    outerSumListy, printIL2D];

  Return[{integrandResultx, integrandResulty}]
] (* end Module  integrandLeftDerList2D *)

(* ##########        Function: integrandLeftDerList3D        ########## *)

(************************************************************************)
(* integrandLeftDerList3D[expression, dependentVar, indepVarList]       *)
(* Purpose:  To find the integrand for the 3-D homotopy operator        *)
(*           using the form where the derivatives of the dependent      *)
(*           varialble have been propagated outside the inner most sum  *)
(*           (after Kruskal et. al.) In this version, operators are     *)
(*           determined by manipulating  lists                          *)
(* Input:    An expression                                              *)
(*           A number for the dependent variable                        *)
(*           A list of independent variables                            *)
(* Output:   The result of applying the higher Euler operators to the   *)
(*           expression.                                                *)
(* Created:  2 March, 2006 by DP                                        *)
(* Code is in File:  newhomotopyoper/hoingr3d.m                         *)
(* Last Modified: 9 April, 2008, 12:43 by DP at CSM                     *)
(************************************************************************)

integrandLeftDerList3D[expression_, dependentVar_, indepVarList_] :=
Module[{derivativeList, orderList, maxix, maxiy, maxiz, partialWRTpartialList,
    innerSumListx, innerSumListy, innerSumListz, ix, iy, iz, kx, ky, kz,
    innerSumx, innerSumy, innerSumz, positionNonZeroTermsx,
    positionNonZeroTermsy, positionNonZeroTermsz, listWOzerosx,
    listWOzerosy, listWOzerosz, dummyderiv, indVar1 = Part[indepVarList, 1],
    indVar2 = Part[indepVarList, 2], indVar3 = Part[indepVarList, 3],
    outerSumListx, outerSumListy, outerSumListz, integrandResultx,
    integrandResulty, integrandResultz, printIL3D},

  If[debugIntegrandLeftDerList, printIL3D = Print, Clear[printIL3D],
      Clear[printIL3D]];
      
  printIL3D["debug code: IL3D, Function: integrandLeftDerList3D,"<>
      " File: integrandLeftDerList3D.m"];

  printIL3D["Computing the integrand for the 3-D homotopy operator."];
  printIL3D["At IL3D IN, the expression given is:"];
  printIL3D[expression];
  printIL3D["At IL3D IN, the current dependent variable is"<>
      " u[", dependentVar, "]."];
  printIL3D["At IL3D IN, the independent variables are ", indepVarList];

  (* Create a list of patial derivatives existing in the expression.    *)
  derivativeList = Union[Cases[expression,
      Derivative[___][u[dependentVar]][indVar1, indVar2, indVar3],
      {0, Infinity}]];
  printIL3D["At IL3D 1, the following list contains derivative terms from"<>
      " the given expression:"];
  printIL3D[derivativeList];

  (* Create lists of the orders of all derivatives that exist in the    *)
  (* expression.                                                        *)
  orderList = Map[Head, Map[Head, derivativeList]] /. Derivative -> List;
  printIL3D["At IL3D 2, a list of orders for the derivative terms shown"<>
      " above with orders for each variable listed in the form"<>
      " {", indVar1, ", ", indVar2, ", ", indVar3, "} is:"];
  printIL3D[orderList];

  (* Determine the highest order derivative for each variable.          *)
  maxix = Max[Map[Part[#, 1] &, orderList], 0];
  maxiy = Max[Map[Part[#, 2] &, orderList], 0];
  maxiz = Max[Map[Part[#, 3] &, orderList], 0];
  printIL3D["At IL3D 3, the order for the expression in"<>
      " variable ", indVar1, " is ", maxix, ",\nthe order for the"<>
      " expression in variable ", indVar2, " is ", maxiy, ",\nand the"<>
      " order for the expression in variable ", indVar3, " is ", maxiz, "."];

  (* Create lists for each variable of the partial derivatives of the   *)
  (* expression with respect to the partial derivative list created     *)
  (* above.                                                             *)
  partialWRTpartialList = Map[D[expression, #] &, derivativeList];
  printIL3D["At IL3D 4, the partial derivatives of the expression with"<>
      " respect to the terms given in the derivative list above are:"];
  printIL3D[partialWRTpartialList];

  (* Create lists of terms for the inner sum of the higher Euler        *)
  (* formula using the partial derivative list created in the last step.*)
  innerSumListx = Table[Map[(-1)^(Part[#, 1] + Part[#, 2] +
      Part[#, 3] - ix - iy - iz - 1)*
      Binomial[Part[#, 1] + Part[#, 2] + Part[#, 3] - ix - iy - iz - 1,
          Part[#, 1] - ix - 1]*
      Binomial[Part[#, 2] + Part[#, 3] - iy - iz,  Part[#, 2] - iy]/
      (Binomial[Part[#, 1] + Part[#, 2] + Part[#, 3], Part[#, 1]]*
      Binomial[Part[#, 2] + Part[#, 3], Part[#, 3]]) &,
      Select[Select[ Select[orderList,
          Part[#, 1] > ix &], Part[#, 2] >= iy &], Part[#, 3] >= iz &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, {_, _, _}?(Part[#1, 1] > ix && Part[#1, 2] >= iy
          && Part[#, 3] >= iz &)]],
      Map[{indVar1, Part[#, 1] - ix - 1} &, Select[Select[Select[orderList,
          Part[#, 1] > ix &], Part[#, 2] >= iy &], Part[#, 3] >= iz &]],
      Map[{indVar2, Part[#, 2] - iy} &, Select[Select[Select[orderList,
          Part[#, 1] > ix &], Part[#, 2] >= iy &], Part[#, 3] >= iz &]],
      Map[{indVar3, Part[#, 3] - iz} &, Select[Select[Select[orderList,
          Part[#, 1] > ix &], Part[#, 2] >= iy &], Part[#, 3] >= iz &]]]] /.
      dummyderiv -> D, {iz, 0, maxiz}, {iy, 0, maxiy}, {ix, 0, maxix - 1}];
  printIL3D["At IL3D 5x, a list of terms for the inner sum for"<>
      " variable ", indVar1, " is:"];
  printIL3D[innerSumListx];

  innerSumListy = Table[Map[(-1)^(Part[#, 1] + Part[#, 2] +
      Part[#, 3] - ix - iy - iz - 1)*
      Binomial[Part[#, 1] + Part[#, 2] + Part[#, 3] - ix - iy - iz - 1,
          Part[#1, 2] - iy - 1]*
      Binomial[Part[#, 1] + Part[#, 3] - ix - iz,  Part[#, 3] - iz]/
      (Binomial[Part[#, 1] + Part[#, 2] + Part[#, 3], Part[#, 2]]*
      Binomial[Part[#, 1] + Part[#, 3], Part[#, 3]]) &,
      Select[Select[Select[orderList, Part[#, 1] >= ix &],
          Part[#, 2] > iy &], Part[#, 3] >= iz &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, {_, _, _}?(Part[#, 1] >= ix && Part[#, 2] > iy
          && Part[#, 3] >= iz &)]],
      Map[{indVar1, Part[#, 1] - ix} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] > iy &], Part[#, 3] >= iz &]],
      Map[{indVar2, Part[#, 2] - iy - 1} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] > iy &], Part[#, 3] >= iz &]],
      Map[{indVar3, Part[#, 3] - iz} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] > iy &], Part[#, 3] >= iz &]]]] /.
      dummyderiv -> D, {iz, 0, maxiz}, {iy, 0, maxiy - 1}, {ix, 0, maxix}];
  printIL3D["At IL3D 5y, a list of terms for the inner sum for"<>
      " variable ", indVar2, " is:"];
  printIL3D[innerSumListy];

  innerSumListz = Table[Map[(-1)^(Part[#, 1] + Part[#, 2] +
      Part[#, 3] - ix - iy - iz - 1)*
      Binomial[Part[#1, 1] + Part[#, 2] + Part[#, 3] - ix - iy - iz - 1,
          Part[#, 3] - iz - 1]*
      Binomial[Part[#, 1] + Part[#, 2] - ix - iy,  Part[#, 1] - ix]/
      (Binomial[Part[#, 1] + Part[#, 2] + Part[#, 3], Part[#, 3]]*
      Binomial[Part[#, 1] + Part[#, 2], Part[#, 1]]) &,
      Select[Select[Select[orderList, Part[#, 1] >= ix &],
          Part[#, 2] >= iy &], Part[#, 3] > iz &]]*
      Thread[dummyderiv[Extract[partialWRTpartialList,
      Position[orderList, {_, _, _}?(Part[#, 1] >= ix && Part[#, 2] >= iy
           && Part[#, 3] > iz &)]],
      Map[{indVar1, Part[#, 1] - ix} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] >= iy &], Part[#, 3] > iz &]],
      Map[{indVar2, Part[#, 2] - iy} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] >= iy &], Part[#, 3] > iz &]],
      Map[{indVar3, Part[#, 3] - iz - 1} &, Select[Select[Select[orderList,
          Part[#, 1] >= ix &], Part[#, 2] >= iy &], Part[#, 3] > iz &]]]] /.
      dummyderiv -> D, {iz, 0, maxiz - 1}, {iy, 0, maxiy}, {ix, 0, maxix}];
  printIL3D["At IL3D 5z, a list of terms for the inner sum for"<>
      " variable ", indVar3, " is:"];
  printIL3D[innerSumListz];

  (* Add the terms in the sum lists to produce lists of inner sums      *)
  (* corresponding to each outer index for ix and iy.                   *)
  innerSumx = Expand[Apply[Plus, innerSumListx, {3, 3}]];
  printIL3D["At IL3D 6x, after adding all inner sum terms for"<>
      " variable ", indVar1, ":"];
  printIL3D[innerSumx];

  innerSumy = Expand[Apply[Plus, innerSumListy, {3, 3}]];
  printIL3D["At IL3D 6y, after adding all inner sum terms for"<>
      " variable ", indVar2, ":"];
  printIL3D[innerSumy];

  innerSumz = Expand[Apply[Plus, innerSumListz, {3, 3}]];
  printIL3D["At IL3D 6z, after adding all inner sum terms for"<>
      " variable ", indVar3, ":"];
  printIL3D[innerSumz];

  (* Remove all zeros from the lists of sums above and keep track of    *)
  (* the positions of the remaining terms.  The positions give the      *)
  (* outer index, ix, iy, and iz, for each term (The positions in the   *)
  (* list are given in the order z, y, x).                              *)
  positionNonZeroTermsx = Map[Part[#] - {1, 1, 1} &,
      Position[innerSumx, _?(# =!= 0 && # =!= List &), {3, 3}]];
  printIL3D["At IL3D 7x, the positions of all nonzero terms in the inner"<>
      " sum list for ", indVar1, " shifted by {1, 1, 1}:"];
  printIL3D[positionNonZeroTermsx];

  positionNonZeroTermsy = Map[Part[#] - {1, 1, 1} &,
      Position[innerSumy, _?(# =!= 0 && # =!= List &), {3, 3}]];
  printIL3D["At IL3D 7y, the positions of all nonzero terms in the inner sum"<>
      " sum list for ", indVar2, " shifted by {1, 1, 1}:"];
  printIL3D[positionNonZeroTermsy];

  positionNonZeroTermsz = Map[Part[#] - {1, 1, 1} &,
      Position[innerSumz, _?(# =!= 0 && # =!= List &), {3, 3}]];
  printIL3D["At IL3D 7z, the positions of all nonzero terms in the inner sum"<>
      " sum list for ", indVar3, " shifted by {1, 1, 1}:"];
  printIL3D[positionNonZeroTermsz];

  listWOzerosx = Flatten[DeleteCases[innerSumx, 0, 3]];
  printIL3D["At IL3D 8x, the inner sum list for ", indVar1, " after"<>
      " removing zero terms:"];
  printIL3D[listWOzerosx];

  listWOzerosy = Flatten[DeleteCases[innerSumy, 0, 3]];
  printIL3D["At IL3D 8y, the inner sum list for ", indVar2, " after"<>
      " removing zero terms:"];
  printIL3D[listWOzerosy];

  listWOzerosz = Flatten[DeleteCases[innerSumz, 0, 3]];
  printIL3D["At IL3D 8z, the inner sum list for ", indVar3, " after"<>
      " removing zero terms:"];
  printIL3D[listWOzerosz];

  (* Multiply the sum by the dependent variable and conduct derivatives *)
  (* with respect to x and y according to the outer sum.  The result is *)
  (* a list of terms.                                                   *)
  outerSumListx = Thread[Times[Map[
      Binomial[Part[#, 3] + Part[#, 2] + Part[#, 1], Part[#, 3]]*
      Binomial[Part[#, 2] + Part[#, 1], Part[#, 2]] &,
      positionNonZeroTermsx]*Map[Derivative[
      Part[#, 3], Part[#, 2], Part[#, 1]][u[dependentVar]]
          [indVar1, indVar2, indVar3] &,
      positionNonZeroTermsx], listWOzerosx]];
  printIL3D["At IL3D 9x, a list of outer sum terms for"<>
      " variable ", indVar1, " is:"];
  printIL3D[outerSumListx];

  outerSumListy = Thread[Times[Map[
      Binomial[Part[#, 3] + Part[#, 2] + Part[#, 1], Part[#, 2]]*
      Binomial[Part[#, 3] + Part[#, 1], Part[#, 1]] &,
      positionNonZeroTermsy]*Map[Derivative[
      Part[#, 3], Part[#, 2], Part[#, 1]][u[dependentVar]]
          [indVar1, indVar2, indVar3] &,
      positionNonZeroTermsy], listWOzerosy]];
  printIL3D["At IL3D 9y, a list of outer sum terms for"<>
      " variable ", indVar2, " is:"];
  printIL3D[outerSumListy];

  outerSumListz = Thread[Times[Map[
      Binomial[Part[#, 3] + Part[#, 2] + Part[#, 1], Part[#, 1]]*
      Binomial[Part[#, 3] + Part[#, 2], Part[#, 3]] &,
      positionNonZeroTermsz]*Map[Derivative[
      Part[#, 3], Part[#, 2], Part[#, 1]][u[dependentVar]]
          [indVar1, indVar2, indVar3] &,
      positionNonZeroTermsz], listWOzerosz]];
  printIL3D["At IL3D 9z, a list of outer sum terms for"<>
      " variable ", indVar3, " is:"];
  printIL3D[outerSumListz];

  (* Add the terms from the lists in the last step to get the result of *)
  (* applying the higher Euler operator for each variable.              *)
  integrandResultx = Expand[Apply[Plus, outerSumListx]];
  integrandResulty = Expand[Apply[Plus, outerSumListy]];
  integrandResultz = Expand[Apply[Plus, outerSumListz]];
  printIL3D["At IL3D 11, the integrands for the 3-D homotopy operator for"<>
      " dependent variable u[", dependentVar, "] are as follows:"];
  printIL3D["For variable ", indVar1, ": ", integrandResultx];
  printIL3D["For variable ", indVar2, ": ", integrandResulty];
  printIL3D["For variable ", indVar3, ": ", integrandResultz];

  (* Clear all variables not being passed back.                         *)
  Clear[derivativeList, orderList, maxix, maxiy, maxiz, partialWRTpartialList,
    innerSumListx, innerSumListy, innerSumListz, ix, iy, iz, kx, ky, kz,
    innerSumx, innerSumy, innerSumz, positionNonZeroTermsx, indVar2, indVar3,
    positionNonZeroTermsy, positionNonZeroTermsz, listWOzerosx,
    listWOzerosy, listWOzerosz, dummyderiv, indVar1, outerSumListx,
    outerSumListy, outerSumListz, printIL3D];

  Return[{integrandResultx, integrandResulty, integrandResultz}]

] (* end Module integrandLeftDerList3D *)

(* ##########          Function: removeCurlTerms           ########## *)

(**********************************************************************)
(* removeCurlTerms[resultFromHomotopy, independVarlist]               *)
(* Purpose: To reduce the number of terms in the final homotopy       *)
(*          operator result to least amount possible.  This is a      *)
(*          modified form of extractSystemForCoefficient              *)
(* Input:   The result from the homotopy operator                     *)
(*          A list of independent variables                           *)
(* Output:  A simplified result                                       *)
(* Created: 20 September 2006 by DP at home                           *)
(* Code is in File:  newhomotopyoper/horecute.m                       *)
(* Major modifications to original file hosimhom.m                    *)
(*          made 18 May, 2007 by DP                                   *)
(* Last Modified:  8 June, 2009, 11:23 by DP at CSM                   *)
(**********************************************************************)

removeCurlTerms[resultFromHomotopy_, allIndependVarList_] :=
Module[{lengthIndVar, lengthComp, unknownConstants,  independVarList,
    homotopyExpnWithConstants, divergenceHomotopyWithK, divCheckDiv,
    divergenceHomotopyNoK, system, zeroEquations, zeroConstants,
    j1 = 1, j2, nonzeroConstants, solveConstantList, systemSoln,
    finalHomotopy, keepHomotopy, divCheck, ivar, i, i1 = 1, printRCT,
    homotopyResultPart, commonDenominator},

  If[debugRemoveCurlTerms, printRCT = Print, Clear[printRCT], Clear[printRCT]];
  printRCT["debug code: RCT, Function: removeCurlTerms File: horecute.m"];

  printRCT["At RCT IN, the vector from homotopy operator:"];
  printRCT[resultFromHomotopy];
  printRCT["At RCT IN, the list of independent variables is ",
      allIndependVarList];

  (* If no result is given, return immediately.                         *)
  If[resultFromHomotopy === {}, Return[resultFromHomotopy]];

  Clear[k];

  printOUT["The homotopy operator has finished inverting the divergence. "<>
      " The program is beginning the removal of curl terms."];

  (* Determine the number of independent variables.                     *)
  independVarList = Part[allIndependVarList, 1];
  lengthIndVar = Length[resultFromHomotopy];

  (* Assign independent variables to consistent terms.                  *)
  ivar[1] = Part[independVarList, 1];
  If[lengthIndVar > 1,  ivar[2] = Part[independVarList, 2]];
  If[lengthIndVar > 2,  ivar[3] = Part[independVarList, 3]];

  (* Find the number of terms in each component of the homotopy result. *)
  lengthComp[1] = If[Head[Part[resultFromHomotopy, 1]] === Plus,
      Length[Part[resultFromHomotopy, 1]], 1];
  lengthComp[2] = If[Head[Part[resultFromHomotopy, 2]] === Plus,
      Length[Part[resultFromHomotopy, 2]], 1];
  lengthComp[3] = If[lengthIndVar === 3,
      If[Head[Part[resultFromHomotopy, 3]] === Plus,
      Length[Part[resultFromHomotopy, 3]], 1], 0];

  (* If only one term exists in each component, there is no need to     *)
  (* go through the reduction process.                                  *)
  If[lengthComp[1] <= 1 && lengthComp[2] <= 1 && lengthComp[3] <= 1,
    Return[resultFromHomotopy]
  ]; (* end If lengthPart *)

  If[printRCT === Print, globalHomotopyVerbose = True];
  If[globalVerbose === All,
    printOUT["The inverted divergence vector given by the homotopy operator"<>
        " has ", lengthIndVar, " components.  The first component has"<>
        " ", lengthComp[1], If[lengthComp[1] === 1, " term", " terms"],
        If[lengthIndVar === 3, ", "," and "],
        "the second component has ", lengthComp[2],
        If[lengthComp[2] === 1, " term", " terms"],
        If[lengthIndVar === 3, ", and the third component has "<>
        ToString[lengthComp[3]]<>
        If[lengthComp[3] === 1, " term.", " terms."], "."]]
  ]; (* end If globalHomotopyVerbose *)
  printOUT["If the number of terms in the components is large, say several"<>
      " hundred, the removal of curl terms may take a long time."];
  printOUT["------------------------------------------"];

  (* Distribute constants through the terms of the homotopy result.     *)
  {homotopyExpnWithConstants[1], homotopyExpnWithConstants[2],
      homotopyExpnWithConstants[3], unknownConstants} =
      applyConstantsToVector[resultFromHomotopy, lengthIndVar];
  printRCT["At RCT 1, the result from the homotopy operator with constants"<>
      " added to each term:"];
  printRCT[{homotopyExpnWithConstants[1], homotopyExpnWithConstants[2],
      homotopyExpnWithConstants[3]}];

  (* Take the divergence on the original homotopy result and the        *)
  (* homotopy result with constants.                                    *)
  If[lengthIndVar === 2,
    divergenceHomotopyWithK = divergence[{homotopyExpnWithConstants[1],
        homotopyExpnWithConstants[2], 0}, ivar[1], ivar[2], ivar[3]];
    divergenceHomotopyNoK = divergence[Append[resultFromHomotopy, 0],
        ivar[1], ivar[2], ivar[3]]
  ]; (* end If lengthIndVar === 2 *)
  If[lengthIndVar === 3,
    divergenceHomotopyWithK = divergence[{homotopyExpnWithConstants[1],
        homotopyExpnWithConstants[2], homotopyExpnWithConstants[3]},
        ivar[1], ivar[2], ivar[3]];
    divergenceHomotopyNoK = divergence[resultFromHomotopy,
        ivar[1], ivar[2], ivar[3]]
  ]; (* end If lengthIndVar === 3 *)
  divergenceHomotopyNoK = Expand[Factor[divergenceHomotopyNoK]];
  divCheckDiv = divergenceHomotopyNoK;
  printRCT["At RCT 2, after divergence is applied to the homotopy result"<>
      " with constants:"];
  printRCT[divergenceHomotopyWithK];
  printRCT["At RCT 3, after divergence is applied to the homotopy result"<>
      " without constants:"];
  printRCT[divergenceHomotopyNoK];

  (* Check for rational expressions.                                    *)
  denom = MemberQ[resultFromHomotopy, Power[n2__, n1_] /; n1 < 0 /;
      MemberQ[n2, u[_][__], {0, Infinity}] ||
      MemberQ[n2, Derivative[__][u[_]][__], {0, Infinity}], {0, Infinity}];
  printRCT["At RCT 4, the homotopy result is rational:  ", denom];

  If[denom,
    denominatorExpnNoK = Union[Map[Denominator,
        Apply[List, divergenceHomotopyNoK]]];
    denominatorExpnNoK = Apply[PolynomialLCM, denominatorExpnNoK];
    printRCT["At RCT 5, the denominator for the divergence without"<>
        " constants:"];
    printRCT[denominatorExpnNoK];

    divergenceHomotopyWithK = Map[Times[#, denominatorExpnNoK] &,
        divergenceHomotopyWithK];
    divergenceHomotopyWithK = Map[Factor, divergenceHomotopyWithK];
    printRCT["At RCT 6, the denominator from the divergence with no"<>
        " constants is multiplied to the divergence with constants:"];
    printRCT[divergenceHomotopyWithK];

    denominatorExpnWithK = Union[Map[Denominator,
        Apply[List, divergenceHomotopyWithK]]];
    denominatorExpnWithK = Apply[PolynomialLCM, denominatorExpnWithK];
    printRCT["At RCT 7, the denominator for the divergence with constants:"];
    printRCT[denominatorExpnWithK];

    divergenceHomotopyWithK = Map[Times[#, denominatorExpnWithK] &,
        divergenceHomotopyWithK];
    divergenceHomotopyWithK = Map[Factor, divergenceHomotopyWithK];
    printRCT["At RCT 8, the divergence with constants with the denominator"<>
        " removed:"];
    printRCT[divergenceHomotopyWithK];

    divergenceHomotopyNoK = Map[Times[#, denominatorExpnNoK*
        denominatorExpnWithK] &, divergenceHomotopyNoK];
    divergenceHomotopyNoK = Map[Factor, divergenceHomotopyNoK];
    printRCT["At RCT 9, the divergence without constants with the"<>
        " denominator removed:"];
    printRCT[divergenceHomotopyNoK]
  ]; (* end If denom *)

  (* Make a system of equations on the unknown constants.                *)
  (* The first case will try to construct a small system, assuming that  *)
  (* most of the k[i] are zero.  This may lead to an inconsistent system *)
  (* in some situations.  If so, then case two will be applied.          *)
  system =
      makeCoefficientEquations[divergenceHomotopyWithK, unknownConstants,
      divergenceHomotopyNoK, Flatten[allIndependVarList], "caseONE"];
  printRCT["At RCT 10, the system of unknown constants to be solved:"];
  printRCT[system];

  If[Not[MemberQ[system, False, {0, Infinity}]],
    solveConstantList = Cases[system, k[_], {0, Infinity}];
    printRCT["At RCT 11, a list of all constants to solve for. Order is"<>
        " not important in this case:"];
    printRCT[solveConstantList];

    zeroConstants = Complement[unknownConstants, solveConstantList];
    zeroConstants = Map[Rule[#, 0] &, zeroConstants];
    printRCT["At RCT 12, a list of constants taken to be zero:"];
    printRCT[zeroConstants];

    (* Find two sets of solutions by reversing the order of the solve   *)
    (* list.  These will be compared for balance.                       *)
    systemSoln[1] = solveCoefficientSystem[system, solveConstantList,
        zeroConstants];
    printRCT["At RCT 13, the first solutions from the coefficient system"<>
        " solver:"];
    printRCT[systemSoln[1]];

    systemSoln[2] = solveCoefficientSystem[system, Reverse[solveConstantList],
        zeroConstants];
    printRCT["At RCT 14, the alternate solutions from the coefficient system"<>
       " solver:"];
    printRCT[systemSoln[2]];

    If[systemSoln[1] === systemSoln[2], j2 = 1, j2 = 2];
    printRCT["At RCT 15, j2 = ", j2, ". If j2 = 2, the two solutions are"<>
        " different and a comparison will be made."];

    While[j1 <= j2,
      (* Replace constants in the homotopy operator with their solutions. *)
      finalHomotopy[j1] = Flatten[{homotopyExpnWithConstants[1],
          homotopyExpnWithConstants[2],
          homotopyExpnWithConstants[3]}] /. systemSoln[j1];
      printRCT["At RCT 16, the no. ",j1, " homotopy result after constants"<>
          " have been replaced using the system solution:"];
      printRCT[finalHomotopy[j1]];
      j1++
    ]; (* end While *)

    If[j2 === 2,
      keepHomotopy = compareCurlFreeSolutions[finalHomotopy[1],
          finalHomotopy[2]],
    (* else *)
      keepHomotopy = finalHomotopy[1]
    ], (* end If j2 *)
  (* else *)
    printRCT["At RCT 17, the system is inconsistent and cannot be solved."<>
      " A new system must be formed."];

    system = makeCoefficientEquations[divergenceHomotopyWithK,
        unknownConstants, divergenceHomotopyNoK, Flatten[allIndependVarList],
        "caseTWO"];
    printRCT["At RCT 18, the system of unknown constants to be solved:"];
    printRCT[system];

    (* Identify all equations in the system where the right hand side   *)
    (* is equal to zero.                                                *)
    zeroEquations = Cases[system,Equal[_,0], {0,2}];
    printRCT["At RCT 19, a list of equations where the right hand side is 0:"];
    printRCT[zeroEquations];

    (* Make a list of all constants, k[i], in zeroEquations.            *)
    zeroConstants = Union[Cases[zeroEquations, k[_], {0, Infinity}]];
    printRCT["At RCT 20, a list of all constants that are in the equations"<>
        " where the right hand side is 0:"];
    printRCT[zeroConstants];

    (* Make a list of all constants that don't show up in zeroEquations *)
    nonzeroConstants = Complement[unknownConstants, zeroConstants];
    printRCT["At RCT 21, a list of all constants that are NOT in the"<>
        " equations where the right hand side is 0:"];
    printRCT[nonzeroConstants];

    (* Solve the system, solving for the constants that don't appear in *)
    (* the zeroEquations first, then solving for all other constants.   *)
    (* Two different solve orders will be applied to give choices for   *)
    (* the solution.                                                    *)
    solveConstantList = Flatten[Append[nonzeroConstants, zeroConstants]];
    printRCT["At RCT 22, a list of all constants in the order they need"<>
      " to be solved for:"];
    printRCT[solveConstantList];

    systemSoln[1] = solveCoefficientSystem[system, solveConstantList, {}];
    printRCT["At RCT 23, the initial solution to the system:"];
    printRCT[systemSoln[1]];

    (* Reverse the order of the constants from the nonzero equations    *)
    (* and solve the system again.  This gives the alternative.         *)
    solveConstantList = Flatten[Append[Reverse[nonzeroConstants],
        zeroConstants]];
    printRCT["At RCT 24, a list of all constants in the order they need"<>
        " to be solved for with the leading constants reversed in order:"];
    printRCT[solveConstantList];

    systemSoln[2] = solveCoefficientSystem[system, solveConstantList, {}];
    printRCT["At RCT 25, the alternate solution to the system:"];
    printRCT[systemSoln[2]];

    If[systemSoln[1] === systemSoln[2], j2 = 1, j2 = 2];
    printRCT["At RCT 26, j2 = ", j2, ". If j2 = 2, the two solutions are"<>
        " different and a comparison will be made."];

    While[j1 <= j2,
      (* Replace constants in the homotopy operator with their          *)
      (* solutions.                                                     *)
      finalHomotopy[j1] = Flatten[{homotopyExpnWithConstants[1],
          homotopyExpnWithConstants[2],
          homotopyExpnWithConstants[3]}] /. systemSoln[j1];
      printRCT["At RCT 27, the no. ",j1, " homotopy result after constants"<>
            " have been replaced using the system solution:"];
      printRCT[finalHomotopy[j1]];
      j1++
    ]; (* end While *)

    (* Compare simplified results. Report the result with the most      *)
    (* balanced set of terms across the components.                     *)
    If[j2 === 2,
      keepHomotopy = compareCurlFreeSolutions[finalHomotopy[1],
          finalHomotopy[2]],
    (* else *)
      keepHomotopy = finalHomotopy[1]
    ] (* end If j2 *)
  ]; (* end If MemberQ *)

  printRCT["At RCT 28, the final homotopy result is:"];
  printRCT[keepHomotopy];

  (* Do a divergence check on the result after curl terms have been     *)
  (* removed.  If the test fails, return the result given by the        *)
  (* homotopy operator.                                                 *)
  divCheck = divergenceCheck[divCheckDiv, keepHomotopy, independVarList];
  printRCT["At RCT 29, Div[homotopyResult] - Div[curlTermsRemoved] = ",
      divCheck];

  If[divCheck =!= 0 || keepHomotopy === {},
    printOUT["The program was unable to remove curl terms from the vector"<>
        " provided by the homotopy operator.  The result with curl terms"<>
        " is begin returned."];
    keepHomotopy = resultFromHomotopy
   ];(* end divCheck *)

  Clear[lengthIndVar, lengthComp, homotopyResultList, unknownConstants,
      termsWithConstants, homotopyExpnWithConstants, divergenceHomotopyWithK,
      divergenceHomotopyNoK, system, zeroEquations, zeroConstants, j1, j2,
      nonzeroConstants, solveConstantList, systemSoln, finalHomotopy,
      divCheck, commonDenominator, ivar, i, divCheckDiv, printRCT];

  Return[keepHomotopy];
]; (* end Module removeCurlTerms *)

(* ##########     Function: makeCoefficientEquations       ########## *)

(**********************************************************************)
(* makeCoefficientEquations[expressionWithC, unknownCoefList,         *)
(*     expressionNoC, indVarList, case]                               *)
(*          modified form of extractSystemForCoefficients             *)
(* Purpose: To extract a linear system to solve for the coefficients  *)
(*          and match the coefficients to coefficients in an          *)
(*          equivalent system.                                        *)
(* Input:   The divergence of the homotopy expression after           *)
(*              coefficients, c[i], have been added                   *)
(*          The divergence of the homotopy expression with no         *)
(*              coefficients                                          *)
(*          List of unknown coefficients                              *)
(*          A list of independent variables                           *)
(*          A string telling whether the long or short method is      *)
(*              being used                                            *)
(* Output:  A system of coefficient equations                         *)
(* Created: 17 September 2006 by DP at CSM                            *)
(* Code is in File:  newhomotopyoper/homacoeq.m                       *)
(* Last Modified: 10 April, 2008, 9:06 by DP at CSM                   *)
(**********************************************************************)

makeCoefficientEquations[expressionWithK_, unknownCoefList_, expressionNoK_,
    indVarList_, case_String] :=
Module[{expandedExpressionWithK, expandedExpressionNoK, termsToRemove, dco,
    commonTermsListWithK, commonTermsListNoK, coefficientEqns, zeroConstants,
    n1, n2, n3, indVarToZeroRules, excludeList, printMCE},

  If[debugMakeCoefficientEquations, printMCE = Print, Clear[printMCE],
      Clear[printMCE]];
  printMCE["debug code: MCE, Function: makeCoefficientEquations,"<>
      " File: homacoeq.m"];

  expandedExpressionWithK = Expand[expressionWithK];
  expandedExpressionNoK = Expand[expressionNoK];

  printMCE["At MCE IN, the expression with k[i] coming into"<>
      " makeCoefficientsEquations: "];
  printMCE[expandedExpressionWithK];
  printMCE["At MCE IN, unknown coefficients to find: ", unknownCoefList];
  printMCE["At MCE IN, the expression without k[i]:"];
  printMCE[expandedExpressionNoK];
  printMCE["At MCE IN, the independent variable list contains ", indVarList];
  printMCE["At MCE IN, the case is ", case];

  (* Make a list of terms needed to identify common coefficients.        *)
  (* The first case will form a system for the constants where the rhs   *)
  (* is never zero.  This may or may not form a consistent system.       *)
  (* The second case sets up the whole coefficient system and will be    *)
  (* used when the first case fails.                                     *)
  Switch[case,
    "caseONE", termsToRemove =
        Reverse[Union[homotopyStripper[expandedExpressionNoK, indVarList]]],
    "caseTWO", termsToRemove =
        Reverse[Union[homotopyStripper[expandedExpressionWithK, indVarList]]]
  ]; (* end Switch *)
  printMCE["AT MCE 1, terms used as a basis for forming coefficient"<>
      " equations:"];
  printMCE[termsToRemove];

  excludeList = Flatten[{u[_][__], Derivative[__][u[_]][__], indVarList,
      Map[Power[#, _] &, indVarList]}];

  (* Match all terms in the expression with k[i] according to the terms  *)
  (* given in the previous list.                                         *)
  rememberCommonTerms =
  commonTermsListWithK = Apply[Plus, Map[Cases[expandedExpressionWithK,
      Times[___?(Not[MemberQ[#, Apply[Alternatives, excludeList],
      {0, Infinity}]] &), #], {0, 2}] &, termsToRemove], {1}];
  printMCE["AT MCE 2, a list of expressions taken from the divergence with"<>
      " k[i] corresponding to each term in the basis list:"];
  printMCE[commonTermsListWithK];

  (* Match all terms in the expression without c[i] according to the     *)
  (* terms given in the previous list.                                   *)
  expandedExpressionNoK = Map[Times[#, dco] &, expandedExpressionNoK];
  commonTermsListNoK = Apply[Plus, Map[Cases[expandedExpressionNoK,
     Times[___?(Not[MemberQ[#, Apply[Alternatives, excludeList],
      {0, Infinity}]] &), #], {0, 2}] &, termsToRemove], {1}];
  printMCE["AT MCE 3, a list of expressions taken from the divergence that"<>
      " has no k[i] corresponding to each term in the basis list:"];
  printMCE[commonTermsListNoK];

  commonTermsListWithK = Expand[Thread[Times[commonTermsListWithK,
      Map[Power[#, -1] &, termsToRemove]]]];
  printMCE["AT MCE 4, all dependent variables and their derivatives have"<>
      " been removed from the expressions with k[i]:"];
  printMCE[commonTermsListWithK];

  commonTermsListNoK = Expand[Thread[Times[commonTermsListNoK,
      Map[Power[# * dco, -1] &, termsToRemove]]]];
  printMCE["AT MCE 5, all dependent variables and their derivatives have"<>
      " been removed from the expressions with no k[i]:"];
  printMCE[commonTermsListNoK];

  (* Set the corresponding expressions from each list equal to each     *)
  (* other, forming coefficient equations.                              *)
  coefficientEqns = Thread[Equal[commonTermsListWithK, commonTermsListNoK]];
  printMCE["AT MCE 6, a system of equations formed from the coefficient"<>
      " expressions:"];
  printMCE[coefficientEqns];

  If[case === "caseONE",
    remainingTerms = Complement[Apply[List, expandedExpressionWithK],
        rememberCommonTerms /. Plus -> Sequence];
    printMCE["At MCE 7, all terms from the divergence with k[i] that do"<>
        " not form coefficient expressions:"];
    printMCE[remainingTerms];

    zeroConstants = Union[Cases[remainingTerms, k[_], {0, Infinity}]];
    zeroConstants = Map[Rule[#, 0] &, zeroConstants];
    printMCE["AT MCE 8, divergence constants which are set automatically"<>
        " to zero (Note: This may form an inconsistent system.):"];
    printMCE[zeroConstants];

    coefficientEqns = coefficientEqns /. zeroConstants;
    printMCE["AT MCE 9, a shortened system of equations with both sides"<>
        " reduced to coefficients only:"];
    printMCE[coefficientEqns];

    coefficientEqns = coefficientEqns /. Equal[0, _] -> False;
    printMCE["AT MCE 10, 0 = parameter must be changed to false:"];
    printMCE[coefficientEqns]
  ]; (* end If case *)

  Clear[expandedExpressionWithK, expandedExpressionNoK, termsToRemove, r,
      commonTermsListWithK, commonTermsListNoK, zeroConstants, n1, n2, n3,
      tim, printMCE];

  Return[coefficientEqns];
]; (* end Module makeCoefficientEquations *)

(* ##########       Function: solveCoefficientSystem       ########## *)

(**********************************************************************)
(* solveCoefficientSystem[coefSystem, coefOrder, knownZeros]          *)
(* Purpose: To solve a linear system of constants.  In this case, all *)
(*          free constants in the solution will be set to zero        *)
(*          automatically. Zero solutions will be listed.             *)
(* Input:   The system to be solved                                   *)
(*          A list of constants to be solved for given in the order   *)
(*              that they should be solved for                        *)
(*          A list of solutions known to be zero                      *)
(* Output:  The solution to the system with every constant accounted  *)
(*          for.                                                      *)
(* Created: 21 May 2007 by DP at CSM                                  *)
(* Code is in File:  newhomotopyoper/hosocosy.m                       *)
(* Last Modified:  10 March, 2008, 16:05 by DP at CSM                 *)
(**********************************************************************)

solveCoefficientSystem[coefSystem_, coefOrder_List, knownZeros_List] :=
Module[{systemSoln, undeterminedConstants, n1, printSCS},

  If[debugSolveCoefficientSystem, printSCS = Print, Clear[printSCS],
      Clear[printSCS]];
  printSCS["debug code: SCS, Function: solveCoefficientSystem,
      File: hosocosy.m"];

  printSCS["At SCS IN, the given system of constants is:"];
  printSCS[coefSystem];
  printSCS["At SCS IN, the constants to be solved for in the order that"<>
      " they are to be solved:"];
  printSCS[coefOrder];
  printSCS["At SCS IN, the solutions known to be zero:"];
  printSCS[knownZeros];

  (* Solve the system given to the function.                            *)
  systemSoln = Flatten[Solve[coefSystem, coefOrder]];
  printSCS["At SCS 1, the initial solution to the system using the"<>
        " reversed constants:"];
  printSCS[systemSoln];

  (* Collect all undetermined constants.                                *)
  undeterminedConstants = Union[Cases[systemSoln /.
      Rule[_, n1_] -> n1, k[_], {0, Infinity}]];
  printSCS["At SCS 2, all undetermined constants in the system solution:"];
  printSCS[undeterminedConstants];

  (* Formulate rules setting all undetermined constants to zero.        *)
  undeterminedConstants = Map[Rule[#, 0] &, undeterminedConstants];
  printSCS["At SCS 3, all undetermined constants set to zero:"];
  printSCS[undeterminedConstants];

  (* Apply the undetermined constants rules to the solution.            *)
  systemSoln = systemSoln /. undeterminedConstants;
  printSCS["At SCS 4, the solution to the system after undetermined"<>
      " constants have been replaced by zeros:"];
  printSCS[systemSoln];

  (* Make a list showing the solutions to all k[i].                   *)
  systemSoln = Union[systemSoln, undeterminedConstants, knownZeros];
  printSCS["At SCS 5, all solutions listed together:"];
  printSCS[systemSoln];

  Clear[undeterminedConstants, n1, printSCS];

  Return[systemSoln]
]; (* end Module solveCoefficientSystem *)

(* ##########      Function: compareCurlFreeSolutions      ########## *)

(**********************************************************************)
(* compareCurlFreeSolutions[invertedDiv1, invertedDiv2]               *)
(* Purpose: To compare two possibilities for the final form for an    *)
(*          inverted divergence and recommend the one that has the    *)
(*          most balanced solution.                                   *)
(* Input:   A pair of inverted divergences                            *)
(* Output:  The inverted divergence with the most balanced form.      *)
(* Created: 21 May 2007 by DP at CSM                                  *)
(* Code is in File:  newhomotopyoper/hoccfsol.m                       *)
(* Last Modified:  10 March, 2008, 17:13 by DP at CSM                 *)
(**********************************************************************)

compareCurlFreeSolutions[invertedDiv1_, invertedDiv2_] :=
Module[{mylength, lengthCheck, findDiffInLength, lengthDiffs, i1, i2,
    keepHomotopy, printCCFS},

  If[debugCompareCurlFreeSolutions, printCCFS = Print, Clear[printCCFS],
      Clear[printCCFS]];
  printCCFS["debug code: CCFS, Function: compareCurlFreeSolutions,
      File: hoccfsol.m"];

  printCCFS["At CFFS IN, the first inverted divergence is:"];
  printCCFS[invertedDiv1];
  printCCFS["At CFFS IN, the first inverted divergence is:"];
  printCCFS[invertedDiv2];

  (* Compare simplified results. Report the result with the most        *)
  (* balanced set of terms across the components.                       *)
  mylength := If[Head[#] === Plus, Length[#], 1] &;
  lengthCheck[1] = Map[mylength, invertedDiv1];
  lengthCheck[2] = Map[mylength, invertedDiv2];
  printCCFS["At CCFS 1, the first simplification has lengths ", lengthCheck[1],
      " and the second simplification has lengths ", lengthCheck[2]];

  findDiffInLength := Abs[#1 - #2] &;
  lengthDiffs[1] = Table[Part[Append[
      lengthCheck[1], Part[lengthCheck[1], 1]], i1],
      {i2, 1, Length[lengthCheck[1]] - 1}, {i1, i2, i2 + 1}];
  lengthDiffs[2] = Table[Part[Append[
      lengthCheck[2], Part[lengthCheck[2], 1]], i1],
      {i2, 1, Length[lengthCheck[2]] - 1}, {i1, i2, i2 + 1}];
  printCCFS["At CCFS 2, lengths of components paired to find differences"<>
      " lengths for the first simplification are ", lengthDiffs[1],
      " and for the second simplification are ", lengthDiffs[2]];

  lengthDiffs[1] = Max[Apply[findDiffInLength, lengthDiffs[1], {1}]];
  lengthDiffs[2] = Max[Apply[findDiffInLength, lengthDiffs[2], {1}]];
  printCCFS["At CCFS 3, the maximum difference in length for the first"<>
      " simplification is ", lengthDiffs[1], " and for the second"<>
      " simplification is ", lengthDiffs[2]];

  keepHomotopy = If[lengthDiffs[1] <= lengthDiffs[2],
      invertedDiv1, invertedDiv2];
  printCCFS["At CCFS 4, the choice for the final homotopy result:"];
  printCCFS[keepHomotopy];

  Clear[mylength, lengthCheck, findDiffInLength, lengthDiffs, i1, i2,
      printCCFS];

  Return[keepHomotopy]
]; (* end Module compareCurlFreeSolutions *)

(* ##########          Function: homotopyOperator            ########## *)

(************************************************************************)
(* homotopyOperator[expression, indepVariableList, numDepVariables]     *)
(* Purpose:  To integrate a 1D expression or to invert the divergence   *)
(*           operator on a 2D or 3D expression by applying a homotopy   *)
(*           operator.                                                  *)
(* Input:    An expression                                              *)
(*           A list of independent variables                            *)
(*           The number of dependent variables                          *)
(* Output:   The result of applying the higher Euler operators to the   *)
(*           expression.                                                *)
(* Created:  23 March, 2006 by DP                                       *)
(* Code is in File:  conservationlaws/hodriver.m                        *)
(* Last Modified: 8 May, 2008, 9:13 by DP at CSM                        *)
(************************************************************************)

homotopyOperator[expression_, indepVariableList_, numDepVariables_] :=
Module[{integrability, interimResult, newIndVarList, finalResult,
    lengthIV = Length[indepVariableList]},

  If[debugHomotopyDriver, printHO = Print, Clear[printHO], Clear[printHO]];

  printHO["debug code: HO, Function: homotopyOperator,"<>
      " File: homotopyOperator.m"];

  printHO["At HO in, the given expression is:"];
  printHO[expression];
  printHO["At HO in, the independent variable list is ", indepVariableList];
  printHO["At HO in, the number of dependent variables is ", numDepVariables];

  (* If the expression given to the homotopy operator is 0, Return 0.   *)
  If[expression === 0,
    Return[Switch[lengthIV, 1, 0, 2, {0,0}, 3, {0,0,0}]]];

  (* Determine if the expression is integrable (1D) or if it is a       *)
  (* divergence (in 2D and 3D).                                         *)
  integrability = integrabilityTest[expression, numDepVariables,
      indepVariableList];
  printHO["At HO 1, the function is ", integrability];

  (* If the expression is not integrable or not a divergence, stop      *)
  (* the program.                                                       *)
  If[Part[integrability, 1] =!= "Integrable", Abort[]];

  (* If the expression is integrable or is a divergence, pass it to the *)
  (* homotopy operator to integrate it or invert the divergence.        *)

  interimResult = rationalExpressionCheck[expression, indepVariableList,
      numDepVariables];
  printHO["At HO 2, the interim result before removal of curl terms:"];
  printHO[interimResult];

  (* If homotopy operator did not work, return empty list.              *)
  If[interimResult === {},
    Return[{}]
    ]; (* end If interimResult *)

  (* If the interim result is an inverted divergence, remove the curl   *)
  (* terms.                                                             *)
  If[lengthIV > 1,
    newIndVarList = {indepVariableList, {t}};
    finalResult = removeCurlTerms[ExpandAll[interimResult], newIndVarList],
  (* else *)
    finalResult = interimResult
  ]; (* end If lengthIV *)
  printHO["At HO 3, the final result after removal of curl terms:"];
  printHO[interimResult];

  finalResult = If[lengthIV === 1, Part[finalResult, 1],
      finalResult, finalResult];

  printHO["At HO OUT, the inverted "<> If[lengthIV === 1, "total derivative:",
      "divergence:"]];
  printHO[finalResult];

  (* Clear all local variables not being returned.                      *)
  Clear[integrability, interimResult, newIndVarList, lengthIV];

  Return[finalResult]
]; (* end Module homotopyOperator *)

(* #################################################################### *)
(*                                                                      *)
(* ##########           End of Homotopy Operator             ########## *)
(*                                                                      *)
(* #################################################################### *)

(* ##########      Function: printNumericWeightRules       ########## *)

(**********************************************************************)
(* printNumericWeightRules[numericWeightRules]                        *)
(* Purpose: To print the numeric weight rules for the user            *)
(* Authors: Lindsay Auble, Maxi von Eye, and Scott Danford            *)
(* Input:   numericWeightRules                                        *)
(* Output:  Printed to screen (none to driver)                        *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whpnweir.m, Created 2 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dppnweir.m                      *)
(* Last Modified:  30 April, 2008, 15:07 by DP at CSM                 *)
(**********************************************************************)

printNumericWeightRules[numericWeightRules_]:=
Module[{lengthNumericWeightRules, numericWeights, i1},

  numericWeights = numericWeightRules/.{Rule-> List};
  lengthNumericWeightRules = Length[numericWeightRules];

  Print["  *** NUMERIC WEIGHT RULES ***\n",
      "------------------------------------------"
  ];

  For[i1=1, i1<= lengthNumericWeightRules, i1++,
    Print[
    "\t", numericWeights[[i1, 1]], " = " , numericWeights[[i1, 2]], ".\n"];
  ]; (* end For *)
  Print["------------------------------------------"]
]; (* end Module printNumericWeightRules *)

(* ##########     Function: printSymbolicWeightRules       ########## *)

(**********************************************************************)
(* printSymbolicWeightRules[symbolicWeightRules]                      *)
(* Purpose: To print the symbolic  weight rules for the user          *)
(* Authors: Lindsay Auble, Maxi von Eye, and Scott Danford            *)
(* Input:   List of symbolicWeightRules                               *)
(* Output:  Printed to screen (none to another function)              *)
(* Last Modified: 3 June 2004 @ CSM                                   *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whprsysr.m, Created 2 June 2004 at CSM              *)
(* Code is in File:  conservationlaws/dpprsysr.m                      *)
(* Last Modified: 9 May, 2008, 12:20 by DP at CSM                     *)
(**********************************************************************)

printSymbolicWeightRules[symbolicWeightRules_]:=
Module[{lengthSymbolicWeightRules, symbolicWeights, i1},

  symbolicWeights = Flatten[symbolicWeightRules] /. {Rule-> List};
  lengthSymbolicWeightRules = Length[symbolicWeightRules];

  Print["  *** SYMBOLIC WEIGHT RULES ***\n",
      "------------------------------------------"];

  For[i1=1, i1<= lengthSymbolicWeightRules, i1++,
    Print[
    "\t", symbolicWeights[[i1,1]], " = " , symbolicWeights[[i1,2]], ".\n" ];
  ];
  Print["------------------------------------------"]
]; (* end Module printSymbolicWeightRules *)

(* ##########                  printRho                    ########## *)

(**********************************************************************)
(* printRho[rho, message]                                             *)
(* Purpose: to print the rho candidates                               *)
(* Input: List of rhos                                                *)
(* Output: The rhos are printed to the screen                         *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whprirho.m, Created 15 June 2004 at CSM             *)
(* Code is in File:  conservationlaws/dpprirho.m                      *)
(* Major Changes Made: 26 May, 2006, 13:35 by DP at CSM               *)
(* Last Modified:  12 May, 2008, 11:59 by DP at CSM                   *)
(**********************************************************************)

printRho[rho_List, message_] := Module[{numRhos, numTerms, count},

  (* Get the number of candidate densities.                             *)
  numRhos = Length[rho];

  (* print results *)
  printLOW["  *** ", message, " ***"];
  printLOW["------------------------------------------"];
  printLOW["There ", If[numRhos === 1, "is ","are "], numRhos,
        " density candidate",
      If[numRhos =!= 1, "s.", "."]];
  For[count = 1, count <= numRhos, count++,
    printHIGH["Density candidate ", count," is:"];
    printHIGH[pdeform[rho[[count]]]];
    If[Head[Part[rho, count]] =!= Times , (* then *)
       numTerms = Length[Part[rho, count]], (* else *)
       numTerms = 1
      ];
    printLOW["There ", If[numTerms === 1, "is ","are "], numTerms, " term",
      If[numTerms > 1, "s", ""]<> " in candidate density ", count, "."]
  ]; (* end For *)
  Print["------------------------------------------"];

  Clear[numRhos, numTerms, count]
]; (* end Module printRho *)

(* ##########        Function: explicitVerification        ########## *)

(**********************************************************************)
(* explicitVerification[origFnList, density, compatibility, flux,     *)
(*     indepVars]                                                     *)
(* Purpose: To explicitly verify that a density-flux pair is indeed   *)
(*          a conservation law after all work is finished.            *)
(* Input:   The original list of differential funtions (the PDE)      *)
(*          A density                                                 *)
(*          Compatibility conditions on the density                   *)
(*          A flux                                                    *)
(*          A list of independent variables                           *)
(* Output:  Result from verification                                  *)
(* Created: 17 December, 2007 by DP at CSM                            *)
(* Code is in File:  conservationlaws/nwexpver.m                      *)
(* Last Modified:  7 June, 2009, 15:29 by DP at CSM                   *)
(**********************************************************************)

explicitVerification[origFnList_, density_, compatibility_, flux_,
    indepVars_] :=
Module[{conlaw, diffExpnsMultiTerms, solveVars, basicReplaceRules,
    notderRule, numIndVars, derivEnds, derivPattern, currentExpn,
    derivsNeeded, derivsNeededList, depVarlhs, n1, n2, n3, n4, n5,
    newRuleTerms, nextRuleLHS, depVar, matchingRule, noOfDerivs, lengthExpn,
    newRule, newRule2 = {}, listOfNewRules = {}, extIndepVars, printEV},

  If[debugExplicitVerification, printEV = Print, Clear[printEV],
      Clear[printEV]];

  If[globalPrintEV, printEV = Print, Clear[printEV]];
  printEV["debug code:EV, Function: explicitVerification,"<>
      " File: nwexpver.m"];

  printEV["At EV IN, the density given:"];
  printEV[density];
  printEV["At EV IN, the compatibility conditions on the density:"];
  printEV[compatibility];
  printEV["At EV IN, the flux given"];
  printEV[flux];
  printEV["At EV IN, the PDE (system) that the conservation law is based on:"];
  printEV[origFnList];
  printEV["At EV IN, the independent variable list contains ", indepVars];

  conlaw =  Factor[D[density, t] + div[flux]];
  printEV["At EV 1, the sum of the t-derivative applied to the density"<>
      " and the divergence applied to the flux:"];
  printEV[conlaw];

  (* Form replacement rules based on terms with t-derivatives.          *)
  diffExpnsMultiTerms = Cases[origFnList, _Plus];
  printEV["At EV 2, Given differential expressions with more than one term:"];
  printEV[diffExpnsMultiTerms];

  solveVars = Cases[origFnList, Derivative[__, n1_][_][__] /; n1 > 0, {0, 3}];
  printEV["At EV 3, differential terms to be solved for:"];
  printEV[solveVars];

  basicReplaceRules = Flatten[Map[Solve[# == 0, solveVars] &, origFnList]];
  printEV["At EV 4, the replacement rules are:"];
  printEV[basicReplaceRules];

  (* Check for equations in the given PDE with no t-derivatives.            *)
  expnWOtders = Union[Flatten[Map[Cases[origFnList,
      Plus[___, Times[___, Part[#]] | Part[#]], {0, Infinity}]&, solveVars]]];
  printEV["At EV 5,  expressions in the PDE with t-derivatives:"];
  printEV[expnWOtders];

  expnWOtders = Complement[origFnList, expnWOtders];
  expnWOtders = MapAt[Times[-1, #] &, expnWOtders,
      Position[Map[Last, expnWOtders], Times[-1, __], {0, 2}]];
  printEV["At EV 6,  expressions in the PDE without t-derivatives:"];
  printEV[expnWOtders];

  notderRule = Map[Rule[Last[#], -Drop[#, -1]] &, expnWOtders];
  printEV["At EV 7, rules for expressions without t-derivatives:"];
  printEV[notderRule];

  conlaw = Factor[Expand[conlaw /. basicReplaceRules /. notderRule]];
  printEV["At EV 8, after applying the replace rules to the conservation"<>
      " law:"];
  printEV[conlaw];

  (* Form new rules for terms with multiple derivatives that do not     *)
  (* fall in earlier lists.                                             *)
  (* Note: newRuleTerms only looks for functions of u[_].  Other        *)
  (* are ignored.                                                       *)
  newRuleTerms = Union[Cases[conlaw, Derivative[__, n1_][u[_]][__] /; n1 > 0,
      {0, 3}]];
  printEV["At EV 9, new replacement rules need to be constructed for these"<>
      " terms with t-derivatives:"];
  printEV[newRuleTerms];

  While[newRuleTerms =!= {},
    nextRuleLHS = First[newRuleTerms];
    printEV["At EV 10, the next rule will be constructed with this term on"<>
        " the left hand side:"];
    printEV[nextRuleLHS];

    depVar = Part[Head[nextRuleLHS], 1];
    matchingRule = Cases[basicReplaceRules,
        Rule[Derivative[__, n1_][depVar][__], __] /; n1 > 0, {0, 3}];
    printEV["At EV 11, the existing rule on dependent variable ", depVar,
        " which most closely matches the new rule term:"];
    printEV[matchingRule];

    If[matchingRule =!={},
      noOfDerivs = (nextRuleLHS /. Derivative[n1__][_][__] -> {n1}) -
          (Part[matchingRule, 1, 1] /. Derivative[n2__][_][__] -> {n2});
      printEV["At EV 12, the number of derivatives that need to be applied"<>
         " to the matching rule to get the right hand side of the new rule:"];
      printEV[noOfDerivs],
    (* else *)
      noOfDerivs = -1;
    ]; (* end If matchingRule *)

    If[Not[MemberQ[noOfDerivs, n1_ /; n1 < 0, {0, Infinity}]],
      newRule =  Rule[nextRuleLHS, D[Part[matchingRule, 1, 2],
          Apply[Sequence, Thread[List[Flatten[{indepVars, t}], noOfDerivs]]]]];
      printEV["At EV 13, the new replacement rule:"];
      printEV[newRule];

      listOfNewRules = Append[listOfNewRules, newRule];
      printEV["At EV 14, an updated list of new replacement rules:"];
      printEV[listOfNewRules];
    ]; (* end If MemberQ *)

    newRuleTerms = If[newRuleterms =!= {}, Drop[newRuleTerms, 1]]
  ]; (* end While *)

  conlaw = Factor[Expand[conlaw /. listOfNewRules //. Flatten[compatibility]]];
  printEV["At EV 15, after applying the replace rules to the divergence of"<>
     " conservation law:"];
  printEV[conlaw];

  (* Make rules for terms with no t-derivatives.                        *)
  While[expnWOtders =!= {},
    extIndepVars = Append[indepVars, t];
    numIndVars = Length[extIndepVars];
    printEV["At EV 16, the extended independent variable list is ",
        extIndepVars, ".  There are ", numIndVars, " variables in the list"];

    derivEnds = ToExpression[Table["n" <> ToString[i], {i, 1, numIndVars}]];
    derivPattern = Map[Pattern[#, Blank[]] &, derivEnds];
    printEV["At EV 17, dummy variables for constructing replacement rules:"<>
        "\nlhs: ", derivPattern, "\nrhs: ", derivEnds];

    currentExpn = Reverse[Apply[List, First[expnWOtders]]];
    currentExpn = If[MatchQ[Part[currentExpn, 1], Times[-1, __]],
        Map[Times[-1, #] &, currentExpn], currentExpn];
    lengthExpn = Length[currentExpn] - 1;
    printEV["At EV 18,  the current expression to be changed into a rule:"];
    printEV[currentExpn];
    printEV["The expression has ", lengthExpn + 1, " terms."];

    currentExpn = Apply[Plus, Partition[currentExpn, lengthExpn, lengthExpn,
        {lengthExpn, 1}, 0], {1}];
    printEV["At EV 19, the current expression with the first term separated"<>
        " from the rest:"];
    printEV[currentExpn];

    derivsNeeded = derivEnds - (Part[currentExpn, 1] /.
        Derivative[Apply[Sequence, derivPattern]][u[_]][__] -> derivEnds);
    derivsNeededList = Thread[List[extIndepVars, derivsNeeded]];
    printEV["At EV 20, derivatives needed on the right-hand side of the"<>
        " rule:"];
    printEV[derivsNeededList];

    depVarlhs = Part[currentExpn, 1] /. Derivative[__][u[n5_]][__] -> n5;
    printEV["At EV 21, the left-hand side of the rule acts on dependent"<>
        " variable ", depVarlhs];

    newRule2 = Derivative[Apply[Sequence, derivPattern]][u[depVarlhs]]
        [Apply[Sequence, extIndepVars]] /;
        Evaluate[Reduce[Apply[And, Map[GreaterEqual[#, 0] &, derivsNeeded]],
        derivEnds]] -> D[-Part[currentExpn, 2],
        Apply[Sequence, derivsNeededList]];
    printEV["At EV 22, a rule for terms without t-derivatives:"];
    printEV[newRule2];

    expnWOtders = Drop[expnWOtders, 1]
  ]; (* end While *)

  conlaw = Factor[Expand[conlaw /. newRule2]];
  printEV["At EV 23, after applying the replace rules to the divergence of"<>
      " the conservation law:"];
  printEV[conlaw];

  conlaw = Factor[Expand[conlaw /. Union[basicReplaceRules, listOfNewRules]]];
  printEV["At EV 23, after reapplying all t-derivative rules to the"<>
      " divergence of the conservation law:"];
  printEV[conlaw];

  (* Clear all local variables not being returned.                      *)
  Clear[diffExpnsMultiTerms, solveVars, basicReplaceRules, listOfNewRules,
      newRuleTerms, nextRuleLHS, depVar, matchingRule, noOfDerivs, n1, n2,
      newRule, newRule2, printEV];

  Return[conlaw];
] (* end Module explicitVerification *)

(* ##########         Function: printFinalResults          ########## *)

(**********************************************************************)
(* printFinalResults[pdename, pdenote, finalRhoList, finalJList,      *)
(*     origDiff, allParams, indepVars, userFormRho, numDepVars,       *)
(*     rankRhoIn]                                                     *)
(* Purpose: To print to the screen all density - flux pairs along     *)
(*          with their compatibility conditions and an explicit       *)
(*          verification of the results                               *)
(* Input:   Name of the PDE                                           *)
(*          Notes about the PDE                                       *)
(*          A list of densities paired with their compatibility       *)
(*              conditions                                            *)
(*          A corresponding list of fluxes                            *)
(*          A list of the original PDE(s) given                       *)
(*          A list of all parameters                                  *)
(*          A list of independent variables                           *)
(*          The form of the density given by the user                 *)
(*          The number of dependent variables                         *)
(*          The rank for the density                                  *)
(* Output:  Prints density-flux pairs to the screen.                  *)
(* Created: 14 May 2007 by DP at CSM                                  *)
(* Code is in File:  conservationlaws/nwprfire.m                      *)
(* Last Modified:  17 March, 2009, 10:22 by DP at CSM                 *)
(**********************************************************************)

dateAndTime[] := Module[{date, month},
  date = Date[];
  month = Switch[date[[2]], 1, "January", 2, "February", 3, "March",
      4, "April", 5, "May", 6, "June", 7, "July", 8, "August",
      9, "September", 10, "October", 11, "November", 12, "December"];
  Print[month <> " ", Part[date, 3], ", ", Part[date, 1], ", at ", 
      Part[date, 4], ":", If[Part[date, 5] < 10, 0, ""], Part[date, 5], "."]
] (* end Module *)

printFinalResults[pdename_, pdenote_, finalRhoList_, finalJList_, origDiff_,
    allParams_, indepVars_, userFormRho_, numDepVars_, rankRhoIn_] :=
Module[{j, j1, j2, m1, m2, m3, m4, m5, currentRank, densityDenominators,
    parameterTrouble, testForParamsInDenoms, lthPT, verify,
    rankRho = rankRhoIn, k1 = 1, conlawInfo, printPI},

  If[debugCheckPrintInput, printPI = Print, Clear[printPI],
      Clear[printPI]];

  printPI["Ranks given for the densities are: ", rankRho];
  printPI["Entering printFinalResults with densities:"];
  printPI[Map[finalRhoList, If[Head[rankRho] === List, rankRho, {rankRho}]]];
  printPI["Entering printFinalResults with fluxes:"];
  printPI[Map[finalJList, If[Head[rankRho] === List, rankRho, {rankRho}]]];
  printPI["All parameters in the PDE are: ", allParams];
  printPI["The independent variable list contains ", indepVars];
  printPI["The user given form for the density is:"];
  printPI[userFormRho];

  Print["*************************************************************\n"<>
      "*****************  RESULTS LISTED BY RANK  ******************\n"<>
      "*************************************************************"];
  dateAndTime[];
  Print["Conservation Laws for: ", pdename];
  If[Head[pdenote] === String && pdenote =!= "", Print["NOTE: ", pdenote]];
  Print["*************************************************************"];
  If[Length[rankRho] > 1, Print["In cases where multiple ranks are run, a"<>
      " density may be repeated in this list.  It is also possible that"<>
      " equivalent densities may occur at different ranks.  Therefore the"<>
      " user is advised to run the check for independence of densities"<>
      " when this list is finished."];
      Print["*************************************************************"]];
  While[rankRho =!= {},
    currentRank = If[Head[rankRho] === List, Part[rankRho, 1], rankRho];
    If[finalRhoList[currentRank] =!= {},
      For[j = 1, j <= Length[finalRhoList[currentRank]], j++,
        If[userFormRho =!= {},
          Print["The form of the density given by the user is"];
          Print[Map[pdeform, userFormRho]];
          Print["From the form given by the user, the program can form the"<>
              " normalized density"];
          Print[pdeform[Part[finalRhoList[currentRank], j, 1]]],
        (* else *)
          Print["A normalized density with the rank of ", currentRank, " is"];
          Print[pdeform[Part[finalRhoList[currentRank], j, 1]]]
        ]; (* end If userFormRho *)

        If[allParams =!= {},
          If[Part[finalRhoList[currentRank], j, 2] === {} ||
              Part[finalRhoList[currentRank], j, 2] === {{}},
            Print["   with no constraints on the parameters."],
            Print["   with the constraint(s) "<>
                "", Flatten[Part[finalRhoList[currentRank], j, 2]], ""<>
                " on the given parameters."]
          ]; (* end If Part *)
          densityDenominators = Map[Denominator,
              Apply[List, Part[finalRhoList[currentRank], j, 1]]];
          printPI["At PI 1, a list of denominators in the density:"];
          printPI[densityDenominators];

          testForParamsInDenoms = Union[Cases[densityDenominators,
              Apply[Alternatives, allParams], {0, Infinity}]];
          printPI["At PI 2, a list of parameters found in the denominators:"];
          printPI[testForParamsInDenoms];

          If[testForParamsInDenoms =!= {},
            parameterTrouble = DeleteCases[Map[Equal[#, 0] &,
                densityDenominators], False];
            printPI["At PI 3, the denominators formed into trouble eqns:"];
            printPI[parameterTrouble];

            parameterTrouble = Union[Flatten[Map[Solve[#, testForParamsInDenoms] &,
                parameterTrouble]]];
            printPI["At PI 4, the solution to the equations, i.e., touble"<>
                " forms when:"];
            printPI[parameterTrouble];

            lthPT = Length[parameterTrouble];
            Print["This result does not appear to be defined for the"<>
                " case"<>If[lthPT > 1, "s", ""]<>" where "<>
                "", parameterTrouble, ".\n  "<>" To test "<>
                If[lthPT > 1, "these cases,", "this case,"]<>" rerun"<>
                " the program with the parameter"<>If[lthPT > 1, "s", ""]<>
                " set to the value"<>If[lthPT > 1, "s", ""]<>" shown."]

          ] (* end If testForParamsInDenoms *)
        ]; (* end If allParams *)

        If[Table[variationalDerivativeMultiD[
            Part[finalRhoList[currentRank], j, 1], j1, indepVars],
            {j1, 1, numDepVars}] === Table[0, {j2, 1, numDepVars}],
          Print["This conservation law is trivial since the density"<>
              " is a divergence with respect to the space variables, "<>
              ToString[indepVars]<> ".  The density can be moved into"<>
              " the flux, leaving a density of zero."];
        ]; (* end If Table *)

        If[Part[finalJList[currentRank], j] =!= {},
          If[Length[Apply[Plus, Part[finalJList[currentRank], j]]] < 41,
            Print["The corresponding flux is"];
            Print[pdeform[Part[finalJList[currentRank], j]]],
          (* else *)
            Print["The corresponding flux has more than forty terms, so it"<>
                " is not displayed here.  Type ",
                StyleForm["pdeform[listOfFluxes[["<>ToString[k1]<>"]]]",
                FontWeight -> Bold], " to see the complete flux corresponding"<>
                " to this density."]
          ]; (* end If Length *)

          verify = explicitVerification[origDiff,
              Part[finalRhoList[currentRank], j, 1],
              Part[finalRhoList[currentRank], j, 2],
              Part[finalJList[currentRank], j], indepVars];
          divOrD = If[Length[indepVars] === 1, Subscript["D", "x"], Div];
          Print["Result of explicit verification:  ",
              Subscript[\[Rho], "t"], " + ", divOrD, " J = ", verify],

        (* else *)
          Print["The corresponding flux cannot be determined."]
        ]; (* end If Part finalJList *)
        Print["*************************************************************"];

        (* Put information into a file which can be printed separately  *)
        (* from the program.                                            *)
        conlawInfo = {Part[finalRhoList[currentRank], j, 1],
        Part[finalRhoList[currentRank], j, 2], currentRank,
        Part[finalJList[currentRank], j]};
        listOfCL = Append[listOfCL, conlawInfo];
        If[globalRunTest, testListOfCL = Append[testListOfCL, conlawInfo]];
        k1++
      ], (* end For *)

    (* else *)
      If[userFormRho === {},
        Print["No nontrivial densities have been found at rank ",
            currentRank, "."];
        If[globalRunTest, testListOfCL = Append[testListOfCL, {{},
            {}, currentRank, {}}]],
      (* else *)
        Print["The expression given by the user does not form a density."]
      ]; (* end If userFormRho *)
      Print["*************************************************************"];
    ]; (* end If finalRhoList[currentRank] *)

    If[Head[rankRho] === List,
      rankRho = Drop[rankRho, 1],
    (* else *)
      rankRho = {}
    ]; (* end If rankRho *)

    listOfFluxes = Append[listOfFluxes, finalJList[currentRank]]

  ]; (* end While *)

  listOfFluxes = Flatten[listOfFluxes, 1];

  (* Clear all local variables not being returned.                      *)
  Clear[j, j1, j2, m1, m2, m3, m4, m5, currentRank, testForParamsInDenoms,
      densityDenominators, parameterTrouble, lthPT, verify, rankRho, printPI]

] (* end Module printFinalResults *)

(* ##########          Function: conservationlaws          ########## *)

(**********************************************************************)
(* conservationlaws                                                   *)
(* Purpose: To print to the screen in Mathematica notation all        *)
(*          density - flux pairs with compatibility conditions        *)
(*          computed by the conservation laws program                 *)
(* Input:   A list containing all information fed in through a        *)
(*              global variable.                                      *)
(* Output:  Prints density-flux pairs to the screen.                  *)
(* Created: 22 May 2008 by DP at CSM                                  *)
(* Code is in File:  conservationlaws/nwprfire.m                      *)
(* Last Modified:  22 May, 2008, 13:47 by DP at CSM                   *)
(**********************************************************************)


conservationlaws := Module[{},
  For[i1 = 1, i1 <= Length[listOfCL], i1++,
    Print["------------------------------------------"];
    Print["Density No. " <> ToString[i1] <> " with rank ",
      Part[listOfCL, i1, 3], ":"];
    Print[Part[listOfCL, i1, 1]];
    If[Part[listOfCL, i1, 2] =!= {},
      Print["The compatibility condition(s) ", Part[listOfCL, i1, 2],
          " have been applied to Density No. " <> ToString[i1] <> "."]];
    Print["The corresponding flux:"];
    Print[Part[listOfCL, i1, 4]]
  ] (* end For *)
] (* end Module conservationLaws *)

(* ##########    Function: independentconservationlaws     ########## *)

(**********************************************************************)
(* independentconservationlaws                                        *)
(* Purpose: To print to the screen in Mathematica notation only       *)
(*          density - flux pairs with compatibility conditions        *)
(*          which are independent.                                    *)
(* Input:   A list containing all information fed in through a        *)
(*              global variable.                                      *)
(* Output:  Prints density-flux pairs to the screen.                  *)
(* Created: 22 May 2008 by DP at CSM                                  *)
(* Code is in File:  conservationlaws/nwprfire.m                      *)
(* Last Modified:  23 May, 2008, 16:24 by DP at CSM                   *)
(**********************************************************************)


independentconservationlaws := Module[{},
  If[listOfIndCL === {}, Return["independentconservationlaws"]];
  For[i1 = 1, i1 <= Length[listOfIndCL], i1++,
    Print["------------------------------------------"];
    Print["Independent Density No. " <> ToString[i1] <> " with rank ",
      Part[listOfIndCL, i1, 4], ":"];
    Print[Part[listOfIndCL, i1, 1]];
    If[Part[listOfIndCL, i1, 2] =!= {},
      Print["The compatibility condition(s) ", Part[listOfIndCL, i1, 2],
          " have been applied to Density No. " <> ToString[i1] <> "."]];
    Print["The corresponding flux:"];
    Print[Part[listOfIndCL, i1, 3]];
    Print["Recheck of explicit verification (",
        Subscript[\[Rho], "t"], " + Div J): ", Part[listOfIndCL, i1, 5]]
  ] (* end For *)
] (* end Module conservationLaws *)

(* ##########            Function: condDriver3D            ########## *)

(**********************************************************************)
(* condDriver3D[diffFunctionList, numDepVariables, indepVariableList, *)
(*              name, parameters, weightParameters, rankRho,          *)
(*              explicit, formRho, userWeightRules]                   *)
(* Purpose: To activate all the functions needed to solve for         *)
(*          conservation laws in multidimensions from systems of      *)
(*          evolution equations                                       *)
(* Input:   List of differential functions                            *)
(*          Number of dependent variables                             *)
(*          List of independent variables                             *)
(*          Name for PDE                                              *)
(*          List of general parameters                                *)
(*          List of weighted parameters (should not be included in    *)
(*          general parameter list)                                   *)
(*          rank for the density to be calculated                     *)
(*          form for the density, if user desires                     *)
(*          List of user weight rules (optional)                      *)
(* Output:  Does not return anything but it will leave behind         *)
(*          variables for inspection, such as finalRhos and finalJs.  *)
(* Adapted From:  data/newzeala/reu2004/reu2004-0811/software/        *)
(*                whdriv3d.m, Created 11 May 2004 at CSM              *)
(* Code is in File:  conservationlaws/ConservationLawsMD              *)
(* Major Changes Made: 14 June, 2006, 5:50 by DP at home              *)
(* Last Modified:  12 November, 2008, 17:27 by DP at CSM              *)
(* Minor change: 6 June, 2024 by UG in Houston (pdeform)              *)
(**********************************************************************)

condDriver3D[diffFunctionList_, numDepVariables_, indepVariableListIn_,
    name_, noteIn_, parametersIn_, weightParametersIn_, rankRhoIn_,
    explicitIn_, formRhoIn_, userWeightRulesIn_] :=
Module[{startTime1, startTime2, note, parameters = parametersIn,
    weightParameters = weightParametersIn, rankRho = rankRhoIn,
    userWeightRules = userWeightRulesIn, explicit = explicitIn,
    formRho = formRhoIn, indepVariableList, rankRhoAdj, evolutionForm,
    evolutionSystem, reverseRules1, reverseRules2, numDepVars, newNote,
    symbolicWeightRules, numericWeightRules, bucketRhos, passRho,
    finalRhos, evolutionRules, finalFluxes, ans, allParameters, printCD3DW},

  (* Begin timing of run. *)
  startTime1 = AbsoluteTime[];
  startTime2 = TimeUsed[];

  If[debugCondDriver3D, printCD3DW = Print, Clear[printCD3DW],
      Clear[printCD3DW]];

  printCD3DW["debug code: CD3DW, Function: condDriver3D, File: dpdriv3d.M"];

  printCD3DW["Starting condDriver3D"];
  printCD3DW["At CD3DW IN, the PDE being evaluated is ", name];
  printCD3DW["At CD3DW IN, the differential function list consists of:"];
  printCD3DW[diffFunctionList];
  printCD3DW["At CD3DW IN, the number of dependent variables is ",
      numDepVariables];
  printCD3DW["At CD3DW IN, the independent space variable list contains ",
      indepVariableListIn];
  printCD3DW["At CD3DW IN, the parameter list contains: ", parametersIn];
  printCD3DW["At CD3DW IN, the weighted parameter list contains: ",
      weightParametersIn];
  printCD3DW["At CD3DW IN, the user has provided weights: "];
  printCD3DW[userWeightRulesIn];
  printCD3DW["At CD3DW IN, the user request rank(s) ", rankRhoIn, " be"<>
      " evaluated."];
  printCD3DW["At CD3DW IN, the user is allowing explicit independent"<>
      " variables in densities up to degree ", explicitIN];
  printCD3DW["At CD3DW IN, the user has provided a form for the density:"];
  printCD3DW[formRhoIn];

  (* Check input for correct defaults.  Parameters and optional items   *)
  (* will automatically be given {} if no information has been given.   *)
  (* User will have to correct equation, dependent variable, and        *)
  (* independent variable information if given incorrectly.             *)
  note = If[Head[noteIn] === String, noteIn, ""];
  parameters = If[Head[parameters] === Symbol, {parameters}, parameters];
  parameters = If[Head[parameters] === List, parameters, {}];
  weightParameters = If[Head[weightParameters] === Symbol,
     {weightParameters}, weightParameters];
  weightParameters = If[Head[weightParameters] === List, weightParameters, {}];
  userWeightRules = If[Head[userWeightRules] === List, userWeightRules, {}];
  rankrho = If[rankRho === {}, Null, rankRho];
  rankRho = If[Head[rankRho] === List && Length[rankRho] === 1,
      Part[rankRho, 1], rankRho, Null];
  rankRho = If[Head[rankRho] === List || NumberQ[rankRho], rankRho, Null];
  explicit = If[explicit === {}, Null, explicit];
  explicit = If[Head[explicit] === List && Length[explicit] === 1,
      Part[explicit, 1], explicit, Null];
  explicit = If[NumberQ[explicit], explicit, Null, Null];
  formRho = If[Head[formRho] === Symbol, {}, formRho,  {}];
  If[formRho === 0 || formRho === {0}, formRho = {}];
  If[formRho =!= {}, rankRho = Null; explicit = Null];
  indepVariableList = DeleteCases[indepVariableListIn, t];
  rankRhoAdj = rankRho;
  printCD3DW["At CD3DW 1, the INPUT information after checks for correct"<>
      " defaults:"];
  printCD3DW["Independent space variable list: ", indepVariableList];
  printCD3DW["Parameters: ", parameters];
  printCD3DW["Weighted Parameters: ", weightParameters];
  printCD3DW["User Provided Weight Rules: ", userWeightRules];
  printCD3DW["Rank of Density: ", rankRho];
  printCD3DW["Explicit Independent Variables: ", explicit];
  printCD3DW["Form of Density: ", formRho];

  (* the Solve function prints out an error statement when it is used:  *)
  (* Solve::svars: Equations may not give solutions for all "solve"     *)
  (* variables.  This turns the error statement off.                    *)
  Off[Solve::"svars"];

  (* Initialize and/or reset global variables.                          *)
  listOfCL = {};
  listOfIndCL = {};
  listOfFluxes = {};
  testListOfCL = {};

  (* Print the chosen system in PDE form.                               *)
  Print["------------------------------------------"];
  Print["  *** PARTIAL DIFFERENTIAL EQUATION"<>
      If[numDepVariables > 1, "S *** ", " *** "]];
  Print["------------------------------------------"];
  printSystemInPdeForm[diffFunctionList, name, note, numDepVariables];
  Print["------------------------------------------"];

  (* Check given PDE for evolution form.                                *)
  evolutionForm = checkForEvolutionForm[diffFunctionList, indepVariableList];
  printCD3DW["The system is in evolution form?  ", evolutionForm];

  (* If equation is not in evolution form, try to rewrite the equation  *)
  (* into a system that is in evolution form.                           *)
  If[evolutionForm === "No",
    {evolutionSystem, formRho, reverseRules1, reverseRules2, numDepVars} =
        putInEvolutionForm[diffFunctionList, formRho, numDepVariables,
        t, name];
    printCD3DW["At CD3DW 2, the rules for reversing the changes needed"<>
        " to produce evolution form:"];
    printCD3DW[reverseRules1];
    printCD3DW[reverseRules2];
    printCD3DW["At CD3DW 3, there are now "<>ToString[numDepVars]<>
        " dependent variables."];
    printCD3DW["At CD3DW 4, the form of the density given by the user has"<>
        " been changed to:"];
    printCD3DW[formRho];

    newNote = StringJoin[note, "\nConservation Laws are reported using the"<>
        " dependent and independent variables given in the original PDE."];

    printLOW["  *** EVOLUTION EQUATION"<>
      If[numDepVars > 1, "S *** ", " *** "]];
    printLOW["------------------------------------------"];
    If[globalVerbose === Minimal || globalVerbose === All,
      printSystemInPdeForm[evolutionSystem, name, note, numDepVars];
    printLOW["------------------------------------------"]
    ], (* end If globalVerbose *)
  (* else *)
    evolutionSystem = diffFunctionList;
    newNote = note;
    numDepVars = numDepVariables
  ];

  allParameters = Union[parameters, weightParameters];
  If[allParameters =!= {},
    Print["Note: The program has been given ", allParameters, " as\n  "<>
        If[Length[allParameters] === 1, "a constant parameter that is",
         "constant parameters that are"] <>" in the given PDE. "<>
        If[weightParameters =!= {}, "The\n  parameter"<>
        If[Length[weightParameters] === 1, " ", "s "]<>
        ToString[weightParameters]<> " will carry a weight.", ""]<>
        " All parameters\n  are assumed to be nonzero"<>
        " throughout the calculations.  If\n  the program determines that a"<>
        " parameter must be zero, it will\n  disregard any conservation laws"<>
        " found under that condition."];
  ]; (* end If parameters *)

  Print["Warning:  All parameters in the given partial differential "<>
      If[Length[diffFunctionList] > 1, "system", "equation"]<>" must be"<>
      " declared in the data file.  The program may not produce correct"<>
      " results if any parameters are left undeclared."];

  If[Length[allParameters] > 3,
    Print["Warning:  If the number of parameters is greater than three, the"<>
        " memory requirements for calculations becomes very large.  To avoid"<>
        " this problem, it may be possible to rescale the given partial"<>
        " differential "<>If[Length[diffFunctionList] > 1, "system",
        "equation"]<>" to reduce the number of parameters.  This may be done"<>
        " by using new parameters on nonlinear or high degree terms and"<>
        " rescaling to eliminate other parameters."]
  ]; (* end If Length *)

  Print["------------------------------------------"];

  (* Calculate the weights. If a candidate for the density is given     *)
  (* by the user, skip this part.                                       *)
  If[formRho === {},
    {symbolicWeightRules, numericWeightRules} = getWeightsWrapper[
        evolutionSystem, weightParameters, numDepVars, userWeightRules];
    printCD3DW["At CD3DW 5, the symbolic weights computed are:"];
    printCD3DW[symbolicWeightRules];
    printCD3DW["At CD3DW 6, the numeric weights computed are:"];
    printCD3DW[numericWeightRules],

  (* else *)
    If[evolutionForm === "Yes",
      bucketRhos = evaluateFormRho[formRho, diffFunctionList, name, parameters,
          weightParameters, indepVariableList];
      printCD3DW["At CD3DW 7, the terms for the density given by the user:"];
      printCD3DW[bucketRhos];
      Print["------------------------------------------"],

    (* else *)
      Print["The feature for checking a density against the PDE only works"<>
          " when the PDE is entered into the data file as an evolution"<>
          " equation (system) as shown directly above. The density to be"<>
          " evaluated must also be entered into the data file under the"<>
          " same transformations used to change the PDE into an evolution"<>
          " equation (system)."];
      Print["All computations are being discontinued."];
      Abort[]
    ] (* end If evolutionForm *)
  ]; (* end If formRho *)

  While[rankRho =!= {},
    passRho = If[Head[rankRho] === List, First[rankRho], rankRho];
    If[formRho === {},
      (* Build candidate densities.                                     *)
      {passRho, bucketRhos} = buildRhoWrapper[symbolicWeightRules,
            numericWeightRules, numDepVars, weightParameters,
            indepVariableList, passRho, explicit];
      printCD3DW["At CD3DW 8, the terms to be used to build densities,"<>
          " grouped into buckets:"];
      printCD3DW[bucketRhos];
      printCD3DW["At CD3DW 9, the current rank is ", passRho],

    (* else *)
      passRho = 0
    ]; (* end If formRho *)

    If[bucketRhos =!= {{}},
      (* Solve for constants if all the weights are non-zero.            *)
      If[Cases[numericWeightRules, weight[_] -> 0] === {},
        {finalRhos[passRho], evolutionRules} =
            solveForCoefficientsWrapper[evolutionSystem, bucketRhos, passRho,
            numDepVars, indepVariableList, weightParameters, parameters];
        printCD3DW["At CD3DW 10, a list of densities paired with their"<>
            " compatibility conditions consist of:"];
        printCD3DW[finalRhos[passRho]];
        printCD3DW["At CD3DW 11, evolution rules for this problem are:"];
        printCD3DW[evolutionRules],

      (* else there are zero weights                                     *)
        (* Add code here to solve ODEs for coefficients.                 *)
        Print["This code is currently not set up to solve for coefficients"<>
            " when there are variables with zero weight."];
        Print["All computations are being discontinued."];
        Abort[]
      ]; (* end If Cases *)

      (* Calculate fluxes for all rhos determined at this point.        *)
      (* Report the results.                                            *)
      If[finalRhos[passRho] =!= {},
        finalRhos[passRho] = Union[finalRhos[passRho]];
        finalFluxes[passRho] = matchDensityWithFlux[
            finalRhos[passRho], evolutionRules, numDepVars,
            indepVariableList, Union[parameters, weightParameters], formRho];
        printCD3DW["At CD3DW 12, a list of fluxes corresponding to the"<>
            " densities shown at CD3DW 7:"];
        printCD3DW[finalFluxes[passRho]],

      (* else *)
        finalFluxes[passRho] = {}
      ]; (* end If finalRhos *)

      (* Remove any modifications made to put the given PDE into        *)
      (* evolution form from the results.                               *)
      If[evolutionForm === "No" && finalRhos[passRho] =!= {},
        {finalRhos[passRho], finalFluxes[passRho]} = reverseEvolutionChanges[
            finalRhos[passRho], finalFluxes[passRho], reverseRules1,
            reverseRules2, indepVariableList];
        printCD3DW["At CD3DW 13, a list of densities corresponding to the"<>
            " densities shown at CD3DW 7 after removing evolution form:"];
        printCD3DW[finalRhos[passRho]];
        printCD3DW["At CD3DW 14, a list of fluxes corresponding to the"<>
            " densities shown at CD3DW 7 after removing evolution form:"];
        printCD3DW[finalFluxes[passRho]]
      ], (* end If evolutionForm *)

    (* else *)
      finalRhos[passRho] = {};
      finalFluxes[passRho] = {}
    ]; (* end If bucketRhos *)

    If[Head[rankRho] === List,
      rankRho = Drop[rankRho, 1],
    (* else *)
      rankRho = {}
    ]; (* end If rankRho *)
    printCD3DW["At CD3DW 15, rankRho has been changed to: ", rankRho]
  ]; (* end While *)

  rankRhoAdj = If[rankRhoAdj === Null, passRho, rankRhoAdj];

  (* Print out results.                                                 *)
  printFinalResults[name, newNote, finalRhos, finalFluxes, diffFunctionList,
      Union[parameters, weightParameters], indepVariableList, formRho,
      numDepVariables, rankRhoAdj];

  (* Check for independence if multiple ranks are supplied, or if a     *)
  (* form of the density is given to the program.                       *)
  If[Length[listOfCL] > 1,
    If[Head[rankRho] === List || formRho =!= {},
      ans = Input["Several densities have been computed by the program"<>
          " at different ranks.  Equivalent or dependent densities may be"<>
          " computed at different ranks.  Would the user like to have these"<>
          " densities analyzed for equivalence and independence? (Yes or No)"];
      If[MemberQ[{"y", "yy", "yes"}, ToLowerCase[ToString[ans]]],
        analyzeDensitiesForIndependence[listOfCL, indepVariableList,
            numDepVariables, {}, diffFunctionList, name]
      ] (* end If MemberQ *)
    ] (* end If Head[rankRho] *)
  ]; (* end If Length[listOfCL] *)

  (* End timing of run. *)
  Print["-------------------------------------------------------"];
  Print["The CPU time to run this case: ", TimeUsed[] - startTime2, " "<>
      "seconds. "];
  Print["The actual time to run this case: ", AbsoluteTime[] - startTime1, ""<>
      " seconds."];

  (* Clear all local variables not being returned.                      *)
  Clear[startTime1, startTime2, note, parameters, weightParameters, rankRho,
      userWeightRules, explicit, formRho, indepVariableList, rankRhoAdj,
      evolutionForm, evolutionSystem, reverseRules1, reverseRules2, numDepVars,
      newNote, symbolicWeightRules, numericWeightRules, bucketRhos, passRho,
      finalRhos, evolutionRules, finalFluxes, ans, printCD3DW];

  (* Clear all input global variables.                                  *)
  Clear[diffFunctionListINPUT, numDependentVariablesINPUT, nameINPUT,
      independentVariableList, noteINPUT, parametersINPUT, formRhoINPUT,
      weightedParametersINPUT, userWeightRulesINPUT, rankRhoINPUT,
      explicitIndependentVariablesInDensitiesINPUT]
]; (* end Module condDriver3D *)

(* The list of Options controls print  and debug statements.            *)
Options[ConservationLawsMD] = {Verbose -> None, printCondDriver3D -> False,
    printHomotopyDriver -> False, printAnalyzeDensities -> False,
    printStripper -> False, printPutInEvolutionForm -> False,
    printGetWeightsWrapper -> False, printComputeSymbolicWeights -> False,
    printEvaluateSymbolicRules -> False, printSolveWeightBalances -> False,
    printGenerateWeightOfTermsList -> False, printEquateTerms -> False,
    printResetWeightSystemForUser -> False, printBuildRhoWrapper -> False,
    printScanForFreeParameters -> False, printPromptUserForWeights -> False,
    printFindSuitableWeightSystem -> False, printCheckNumericWeights -> False,
    printEvaluateWeightsForFreeParameters -> False, printReduceRho -> False,
    printScaleWeightsIfFractions -> False, printBuildFullRho -> False,
    printBuildFullRhoList -> False, printConstructInitialRhoTerms -> False,
    printDetermineDerivativesNeeded -> False, printReduceRhoEuler -> False,
    printAttachExplicitVars -> False, printEvaluateFormRho -> False,
    printCheckFormRhoAsDensity -> False,
    printSolveWrapper -> False, printGetRhoCoefficientsSystem -> False,
    printReplaceOnTheEquation -> False, printEvolutionRules -> False,
    printExtractCoefficientSystem -> False, printGeneralSolver -> False,
    printFactorSign -> False, printAnalyzeForZeroConstants -> False,
    printFreeRhoConstants -> False, printZeroFactorStripper -> False,
    printRemoveDuplicateTerms -> False, printEvaluateRho -> False,
    printExpressionSimplify -> False, printNewNormalize -> False,
    printSeparateRhos -> False, printCheckForDivergences -> False,
    printMatchDensityWithFlux -> False, printReverseEvolutionChanges -> False,
    printExplicitVerification -> False, printCheckPrintInput -> False,
    printRationalExpressionCheck -> False, printStandAloneOutput -> True,
    printIntegrabilityTest -> False, printDegreeOperator -> False,
    printHomotopyStripper -> False, printHomotopyFinalIntegration -> False,
    printDivergenceCheck -> False, printIntegrandLeftDerList -> False,
    printApplyConstantsToVector -> False, printVariationalDerivative -> False,
    printRemoveCurlTerms -> False, printMakeCoefficientEquations -> False,
    printTakeOnlyNumerator -> False, printSolveCoefficientSystem -> False,
    printShiftDenominatorTerm -> False, printCompareCurlFreeSolutions -> False,
    printIndepDriver -> False, printTotalDivergenceCheck -> False,
    printSortDensities -> False, printCheckDensityForEquivs -> False,
    printCheckMultipleDensity -> False, printCheckForDependency -> False,
    printCheckLinearCombInd -> False, printCheckForPartialDependence -> False,
    printReportEquivalence -> False, printReportDensityEvaluation -> False,
    printExtractSystemForCoefficients -> False, testResults -> False};

Verbose::usage = "To use Verbose, type ConservationLawsMD[Verbose -> Option].\n"<>
    "Three options exist for Verbose:\n  None (default)  Only"<>
    " the final results are printed to the screen.\n  Minimal         A"<>
    " minimal amount of information showing the progress\n                 "<>
    " of the algorithm is printed to the screen.\n  All             All key"<>
    " steps are printed to the screen.  This setting\n                  is"<>
    " not recommended for calculations at high ranks\n                  or"<>
    " for calculations on Mathematica 6 or higher."

(* ##########         Function: ConservationLawsMD         ########## *)

(**********************************************************************)
(* ConservationLawsMD[Options]                                        *)
(* Purpose: To adjust program options and to open the menu.           *)
(* Input:   Any changes to options found under                        *)
(*          Options[ConservationLawsMD]                               *)
(* Output:  None                                                      *)
(* Code is in File:  conservationlaws/ConservationLawsMD.m            *)
(* Created: 2 May, 2008, by DP                                        *)
(* Last Modified:  17 April, 2009, 8:51 by DP at CSM                  *)
(**********************************************************************)

ConservationLawsMD[opts___?OptionQ] := Module[{},

  (* Set all debug print statements.  Any print option set to True      *)
  (* turns on all debug statements for that function.                   *)
  debugCondDriver3D = printCondDriver3D /. {opts} /.
      Options[ConservationLawsMD];
  debugStripper = printStripper /. {opts} /. Options[ConservationLawsMD];
  debugPutInEvolutionForm = printPutInEvolutionForm /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckForEvolutionForm = printPutInEvolutionForm /. {opts} /.
      Options[ConservationLawsMD];
  debugReverseEvolutionChanges = printReverseEvolutionChanges /. {opts} /.
      Options[ConservationLawsMD];
  debugGetWeightsWrapper = printGetWeightsWrapper /. {opts} /.
      Options[ConservationLawsMD];
  debugComputeSymbolicWeights = printComputeSymbolicWeights /. {opts} /.
      Options[ConservationLawsMD];
  debugEvaluateSymbolicRules = printEvaluateSymbolicRules /. {opts} /.
      Options[ConservationLawsMD];
  debugGenerateWeightOfTermsList = printGenerateWeightOfTermsList /. {opts} /.
      Options[ConservationLawsMD];
  debugEquateTerms = printEquateTerms /. {opts} /.
      Options[ConservationLawsMD];
  debugSolveWeightBalances = printSolveWeightBalances /. {opts} /.
      Options[ConservationLawsMD];
  debugResetWeightSystemForUser = printResetWeightSystemForUser /. {opts} /.
      Options[ConservationLawsMD];
  debugScanForFreeParameters = printScanForFreeParameters /. {opts} /.
      Options[ConservationLawsMD];
  debugFindSuitableWeightSystem = printFindSuitableWeightSystem /. {opts} /.
      Options[ConservationLawsMD];
  debugEvaluateWeightsParameters = printEvaluateWeightsForFreeParameters /.
      {opts} /. Options[ConservationLawsMD];
  debugPromptUserForWeights = printPromptUserForWeights /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckNumericWeights = printCheckNumericWeights /. {opts} /.
      Options[ConservationLawsMD];
  debugBuildRhoWrapper = printBuildRhoWrapper /. {opts} /.
      Options[ConservationLawsMD];
  debugScaleWeightsIfFractions = printScaleWeightsIfFractions /. {opts} /.
      Options[ConservationLawsMD];
  debugReduceRho = printReduceRho /. {opts} /. Options[ConservationLawsMD];
  debugBuildFullRho = printBuildFullRho /. {opts} /.
      Options[ConservationLawsMD];
  debugBuildFullRhoList = printBuildFullRhoList /. {opts} /.
      Options[ConservationLawsMD];
  debugConstructInitialRhoTerms = printConstructInitialRhoTerms /. {opts} /.
      Options[ConservationLawsMD];
  debugDetermineDerivativesNeeded = printDetermineDerivativesNeeded /.
      {opts} /. Options[ConservationLawsMD];
  debugReduceRhoEuler = printReduceRhoEuler /. {opts} /.
      Options[ConservationLawsMD];
  debugAttachExplicitVars = printAttachExplicitVars /. {opts} /.
      Options[ConservationLawsMD];
  debugEvaluateFormRho = printEvaluateFormRho /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckFormRhoAsDensity = printCheckFormRhoAsDensity /. {opts} /.
      Options[ConservationLawsMD];
  debugSolveWrapper = printSolveWrapper /. {opts} /.
      Options[ConservationLawsMD];
  debugEvolutionRules = printEvolutionRules /. {opts} /.
      Options[ConservationLawsMD];
  debugReplaceOnTheEquation = printReplaceOnTheEquation /. {opts} /.
      Options[ConservationLawsMD];
  debugGetRhoCoefficientsSystem = printGetRhoCoefficientsSystem /. {opts} /.
      Options[ConservationLawsMD];
  debugExtractCoefficientSystem = printExtractCoefficientSystem /. {opts} /.
      Options[ConservationLawsMD];
  debugExtractSystemForCoefficients = printExtractSystemForCoefficients /.
      {opts} /. Options[ConservationLawsMD];
  debugFactorSign = printFactorSign /. {opts} /. Options[ConservationLawsMD];
  debugGeneralSolver = printGeneralSolver /. {opts} /.
      Options[ConservationLawsMD];
  debugAnalyzeForZeroConstants = printAnalyzeForZeroConstants /. {opts} /.
      Options[ConservationLawsMD];
  debugZeroFactorStripper = printZeroFactorStripper /. {opts} /.
      Options[ConservationLawsMD];
  debugFreeRhoConstants = printFreeRhoConstants /. {opts} /.
      Options[ConservationLawsMD];
  debugRemoveDuplicateTerms = printRemoveDuplicateTerms /. {opts} /.
      Options[ConservationLawsMD];
  debugEvaluateRho = printEvaluateRho /. {opts} /.
      Options[ConservationLawsMD];
  debugNewNormalize = printNewNormalize /. {opts} /.
      Options[ConservationLawsMD];
  debugExpressionSimplify = printExpressionSimplify /. {opts} /.
      Options[ConservationLawsMD];
  debugSeparateRhos = printSeparateRhos /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckForDivergences = printCheckForDivergences /. {opts} /.
      Options[ConservationLawsMD];
  debugMatchDensityWithFlux = printMatchDensityWithFlux /. {opts} /.
      Options[ConservationLawsMD];
  debugExplicitVerification = printExplicitVerification /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckPrintInput = printCheckPrintInput /. {opts} /.
      Options[ConservationLawsMD];
  debugHomotopyDriver = printHomotopyDriver /. {opts} /.
      Options[ConservationLawsMD];
  debugHomotopyFinalIntegration = printHomotopyFinalIntegration /. {opts} /.
      Options[ConservationLawsMD];
  debugShiftDenominatorTerm = printShiftDenominatorTerm /. {opts} /.
      Options[ConservationLawsMD];
  debugApplyConstantsToVector = printApplyConstantsToVector /. {opts} /.
      Options[ConservationLawsMD];
  debugIntegrabilityTest = printIntegrabilityTest /. {opts} /.
      Options[ConservationLawsMD];
  debugIntegrandLeftDerList = printIntegrandLeftDerList /. {opts} /.
      Options[ConservationLawsMD];
  debugDegreeOperator = printDegreeOperator /. {opts} /.
      Options[ConservationLawsMD];
  debugVariationalDerivative = printVariationalDerivative /. {opts} /.
      Options[ConservationLawsMD];
  debugDivergenceCheck = printDivergenceCheck /. {opts} /.
      Options[ConservationLawsMD];
  debugRemoveCurlTerms = printRemoveCurlTerms /. {opts} /.
      Options[ConservationLawsMD];
  debugTakeOnlyNumerator = printTakeOnlyNumerator /. {opts} /.
      Options[ConservationLawsMD];
  debugMakeCoefficientEquations = printMakeCoefficientEquations /. {opts} /.
      Options[ConservationLawsMD];
  debugSolveCoefficientSystem = printSolveCoefficientSystem /. {opts} /.
      Options[ConservationLawsMD];
  debugCompareCurlFreeSolutions = printCompareCurlFreeSolutions /. {opts} /.
      Options[ConservationLawsMD];
  debugHomotopyStripper = printHomotopyStripper /. {opts} /.
      Options[ConservationLawsMD];
  debugRationalExpressionCheck = printRationalExpressionCheck /. {opts} /.
      Options[ConservationLawsMD];
  debugAnalyzeDensities = printAnalyzeDensities /. {opts} /.
      Options[ConservationLawsMD];
  debugIndepDriver = printIndepDriver /. {opts} /.
      Options[ConservationLawsMD];
  debugSortDensities = printSortDensities /. {opts} /.
      Options[ConservationLawsMD];
  debugTotalDivergenceCheck = printTotalDivergenceCheck /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckDensityForEquivs = printCheckDensityForEquivs /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckMultipleDensity = printCheckMultipleDensity /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckForDependency = printCheckForDependency /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckLinearCombInd = printCheckLinearCombInd /. {opts} /.
      Options[ConservationLawsMD];
  debugCheckForPartialDependence = printCheckForPartialDependence /. {opts} /.
      Options[ConservationLawsMD];
  debugReportDensityEvaluation = printReportDensityEvaluation /. {opts} /.
      Options[ConservationLawsMD];
  debugReportEquivalence = printReportEquivalence /. {opts} /.
      Options[ConservationLawsMD];

  Clear[printLOW, printHIGH];

  globalVerbose = Verbose /. {opts} /. Options[ConservationLawsMD];
  If[globalVerbose === Minimal, printLOW = Print];
  If[globalVerbose === All, printHIGH = printLOW = Print];

  globalRunTest = testResults /. {opts} /. Options[ConservationLawsMD];

  (* Start the program by calling the menu function.                    *)
  newMenu
] (* end Module ConservationLawsMD *)

(* UG 06/06/2024 *)
(* Print["Code ConservationLawsMD.m of February 4, 2010 was successfully loaded."]; *)
Print["Code ConservationLawsMD.m of February 4, 2010 was successfully loaded."];
Print["Last updated: June 6, 2024 by Unal Goktas at Texas A&M University Houston."];

(* ************************ end of all ****************************** *)
