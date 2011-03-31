BeginPackage["GraphicsTools`",{"PlotLegends`","TimeSeriesManipulation`" }]

ToStaticGraphic::usage="";
Labels::usage="";
LegendRow::usage="";
ScatterPlot::usage="";
DateListPlot2D::usage="";
LegendDateListPlot::usage="";
StackedDateListPlot::usage="";
LabeledBarChart::usage="";
CountryGraphPlot::usage="";

Begin["Private`"];

(* Remove Dynamic elements from graphics *)

ToStaticGraphic[expr_] :=
 expr //. {d : (_Tooltip | _StatusArea) :>
    First@d, _FrontEnd`If | _FEPrivate`If :> {},
   DynamicBox :> Identity}

(* Add Labels *)

$DefaultLabelFont = "Optima";

Options@Labels = {
	"Title" -> None,
	"Text" -> None,
   	"Source" -> None,
   	"SourceFormat" -> (Style[Text@#, 8, "FontFamily" -> $DefaultLabelFont] &),
   	"TitleFormat" -> (Style[Text@#, 12, "FontFamily" -> $DefaultLabelFont] &),
   	"TextFormat" -> (Style[Text@#, 10,  "FontFamily" -> $DefaultLabelFont] &)
   	};

$zeroLabel =
  Graphics[{White, PointSize[0.00001], Point[{0, 0}]}, ImageSize -> 0];

Labels[exp_, rules : OptionsPattern[]] :=
 Fold[Labeled[#1, First@#2, {Last@#2}] &, exp,
  Select[Transpose[{{OptionValue["TitleFormat"][OptionValue["Title"]],
       OptionValue["TextFormat"][OptionValue["Text"]],
      OptionValue["SourceFormat"][
       OptionValue["Source"]], $zeroLabel}, {{Top, Center}, {Bottom,
       Center}, {Bottom, Left}, {Bottom, Center}}}],
   FreeQ[#, None] &]]

(* Add Legend *)


LegendRow[exp_, rules:{__Rule}] :=
 Block[{legendColumn},
  legendColumn = Column[
    Row[{Graphics[{#2, Disk[{0, 0}]}, ImageSize -> 15], Spacer[5],
        Text@Style[#1, Small]}] & @@@ rules];
  Row@{exp, Item[legendColumn, Alignment -> Top]}]

 LegendRow[exp_, list_List]:=
 	With[{cols= ColorData[1, "ColorList"]},
 		LegendRow[exp,
 			MapThread[Rule, {list, Take[cols, Length@list]}]
 		]
 	]

(* New Plots - ScatterPlot *)

ClearAll[ScatterPlot]

Options[ScatterPlot] = {"RegressionLine" -> True, "ColorRules" -> {},
   "Points" -> True, "Labels" -> True,
   "PointDrawingFunction" :> ({#2 /. colorRules, PointSize[0.02],
       Point@#1} &),
   "LabelDrawingFunction" :> (Inset[Style[#2, 5],
       Offset[{11, 0}, #1]] &), "LabelSize" -> 9};

ScatterPlot[data_List, labels_List, opts : OptionsPattern[]] :=
    Quiet[Block[ {x, newopts, g, lm, datapoints, colorRules,line,points, pointLabels},
    	colorRules = Append[OptionValue@"ColorRules", _ -> Lighter@Blue];
              newopts =
              DeleteCases[{opts}, _[
                Alternatives @@ (First /@ Options[ScatterPlot]), _]];
              datapoints =
               Select[Transpose@{data, labels}, FreeQ[#, Missing] &];
              lm = LinearModelFit[datapoints[[All, 1]], x, x];
              line = If[ MatchQ[OptionValue["RegressionLine"],True|Above|Below],
                                {Thickness[0.003], Red,
                                Line[Sort[
                                lm["Data"] - ({0, #} & /@ lm["FitResiduals"])]]},
                                {}
                            ];
              points = If[ OptionValue["Points"],
                     OptionValue["PointDrawingFunction"] @@@ datapoints,
                     {}
                 ];
               pointLabels = If[ OptionValue["Labels"],
                     OptionValue["LabelDrawingFunction"] @@@ datapoints,
                     {}
                 ];
              g = Graphics[
              	Switch[OptionValue["RegressionLine"],
              		Above | True, Append,
              		_ , Prepend][{points,pointLabels}, line],
                newopts, AspectRatio -> 1/GoldenRatio, Axes -> None,
                Frame -> {True, True, False, False},
                PlotRange -> All]
          ], {OptionValue::"nodef"}]

ScatterPlot[rules : {__Rule}, opts : OptionsPattern[]] :=
    ScatterPlot[rules[[All, 2]], rules[[All, 1]], opts]

(* New Plots - DateListPlot2D *)

Options[DateListPlot2D] = {"LabelDisplayFunction" :> (Inset[
       If[ Mod[First@#1, 5] == 0,
           Style[DateString[#1, "Year"], 8],
           ""
       ], Offset[{-10, 0}, #2]] &),
   "PointDisplayFunction" :> ({Lighter@Blue, PointSize[0.015],
       Tooltip[Point@#2, Style[DateString[#1, "Year"], 8]]} &),
   "LineDisplayFunction" -> ({Darker@Blue, Line[#[[All, 2]]]} &)};

DateListPlot2D[data_, opts : OptionsPattern[]] :=
    Quiet[Block[ {datapoints = data,
       dateStringFunction = DateString[#, "Year"] &},
              Graphics[{OptionValue["PointDisplayFunction"] @@@
                 datapoints, OptionValue["LineDisplayFunction"]@datapoints,
                OptionValue["LabelDisplayFunction"]@@@datapoints},
               Sequence @@
                DeleteCases[{opts}, _[
                  Alternatives @@ (First /@ Options[DateListPlot2D]), _]],
               AspectRatio -> 1, Axes -> None,
               Frame -> {True, True, False, False},
               PlotRange -> All]
          ], {OptionValue::"nodef"}]

(* New Plots - LabedledDateListPlot *)

ClearAll[LegendDateListPlot]

Options[LegendDateListPlot] = {"ColorRules" -> Automatic};

LegendDateListPlot[rules_, rest : OptionsPattern[]] :=
 Quiet[Block[{graphic, colorRules, cols = Flatten[Table[ColorData[1, "ColorList"], {2}]]},
  colorRules =
   If[MatchQ[OptionValue@"ColorRules", Automatic],
    Rule @@@ Transpose@{rules[[All, 1]], cols[[;; Length@rules]]},
    OptionValue["ColorRules"]];
  graphic =
   DateListPlot[Tooltip[#2, #1] & @@@ rules,
    Sequence @@ FilterRules[{rest}, Options@DateListPlot],
    MeshStyle -> Directive[PointSize[0.01], Darker@Gray],
    PlotStyle -> (Directive[
         Thickness@0.005, #] & /@ (rules[[All, 1]] /. colorRules)),
    Joined -> True, Mesh -> All, ImageSize -> 400];
  LegendRow[graphic,
   Rule @@@
    Transpose@{rules[[All, 1]], rules[[All, 1]] /. colorRules}]],
    {OptionValue::nodef}]

(* New Plots - StackedDateListPlot *)

StackedDateListPlot[rules_, colorRules_, opts___] :=
    With[ {newrules =
       MapThread[
        Rule, {rules[[All, 1]],
         FoldList[TimeSeriesMapThread[Plus, {#1, #2}] &,
          First[rules[[All, 2]]], Rest[rules[[All, 2]]]]}]},
        LegendDateListPlot[newrules, opts, InterpolationOrder -> 1,
          Mesh -> None, PlotStyle -> Black,
          Filling ->
            Apply[Rule[#1, {{#2}, #3}] &,
             Flatten /@
              Transpose@{Reverse /@
                 Partition[Range[0, Length@colorRules], 2, 1],
                newrules[[All, 1]] /. colorRules}, {1}] /. {0} -> Axis,
          PlotStyle -> (newrules[[All, 1]] /. colorRules), Joined -> True,
          "ColorRules" -> colorRules] /.
         Column[l_List] :> Column[Reverse@l]
    ]

(* New Plots -  Nice Bar Chart *)

LabeledBarChart[rules_, colorRules_,options:OptionsPattern[]]:=
BarChart[
   rules[[All,2]],options,
   ChartStyle -> (rules[[All,1]] /. colorRules),
   ChartLabels -> Placed[rules[[All,1]], Left, Rotate[#, 45 Degree] &],
   BarOrigin -> Left
   ]

(* CountryGraphPlot *)

capitalCityCoordinates :=
  Last@# -> Reverse@CityData[#, "Coordinates"] & /@
   Select[(CountryData[#, "CapitalCity"] & /@ CountryData[]),
    FreeQ[#, _Missing] &];

CountryGraphPlot[graph_] :=
 With[{countries = Union@Flatten[List @@@ graph]},
  Show[Graphics[{EdgeForm[Black], Gray,
     CountryData[#, "Polygon"] & /@ countries}],
   GraphPlot[graph,
    VertexRenderingFunction -> (Inset[CountryData[#2, "Flag"], #1,
        Center, 2] &),
    VertexCoordinateRules -> capitalCityCoordinates]]]

(* CountryGraphPlot Example

CountryGraphPlot@
 Select[Select[
   Union@Flatten[
     Thread[# ->
         Union[CountryData[#, "ImportPartners"],
          CountryData[#, "ExportPartners"]]] & /@ CountryData[]],
   FreeQ[#, _Missing] &],
  FreeQ[#, Alternatives @@
     Complement[CountryData[], CountryData["EuropeanUnion"]]] &]

     *)

End[];
EndPackage[];