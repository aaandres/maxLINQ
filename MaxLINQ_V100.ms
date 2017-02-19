

	/*
	MaxLINQ  -  LINQ functions for MaxScript
	----------------------------------------------
	----------------------------------------------
	Copyright (c) 2017 Andrés Fernández Cruz (PathScripts)
	
	Permission is hereby granted, free of charge, to any person obtaining a copy of this
	software and associated documentation files (the "Software"), to deal in the Software
	without restriction, including without limitation the rights to use, copy, modify, merge,
	publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
	to whom the Software is furnished to do so, subject to the following conditions:
	
	The above copyright notice and this permission notice shall be included in all copies or
	substantial portions of the Software.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
	IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
	OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
	IN THE SOFTWARE.
	----------------------------------------------
	----------------------------------------------
	This maxScript tool is intended to be a way to use inside maxscript language sweet predefined functions
	similar to those in C# LINQ, through an easy lambda-style expressions syntax, concatenating functions after functions.
	You can query arrays, strings, maxscript enumerables (objectSet, bitArray, groups, pathnames...) and DotNet IEnumerables.
	Moreover, it includes  specific functions for maxObjects.
	It can be used both in your own scripts or directly in the 3dsMax Listener.
	NOTE: Querying directly a singleton node will behave as querying the object and its children and subChildren.
	
	Some intuitive simple examples are:
		(_From #(1,2,3,4,5,6,7,8,9))._Where("x => x > 4")	--> returns #(5, 6, 7, 8, 9)
		(_From "Hello World").Contains("World") 			--> returns True
		(_From #{1..9}).Sum("x => x*2") 						--> returns 90
		var = #(1,2,3,4,5); qvar = _From var; qvar.Sum#()	--> returns 15
		var = #(1,2,3,4,5); qvar = _From var; (qvar.TakeWhile("q: x => x < 4")).Average#()	--> returns 2.0
		str = @"C:\Test_Folder\myExample_01.ms"; (_From str)._Where("_")	--> returns #(8, 25)
		str = @"C:\Test_Folder\myExample_01.ms"; (_From str).Last("_")		--> returns 25
		(_From objects).Sort("node => node.name")		--> sort scene objects by name
		
	Copy this maxscript file into your scripts startup folder and enjoy with more than 40 sweet functions
	to query arrays of any type and strings.
	
	For a general overview, help and examples, key in:
	maxLINQ.Help()							--	General overview and list of functions
	maxLINQ.helpFN "<function name>":	--	help on each maxLINQ function and examples
	maxLINQ.helpFN "LA"					--	help on LambdExpressions syntax
	maxLINQ.helpMAN()						--	help on maxLINQ Manager Functions
	----------------------------------------------
	----------------------------------------------
	For highlighting main commands in your maxScript editor, copy the following lines into your MXS_EditorUser.properties
	or add individual commands to your own definitions for 'keywords12':
	
		# keywords12 is for user defined words to be colorized. Keywords are checked for in this list first
		keywords12.$(file.patterns.MAXScript)=\
		_from maxlinq maxlinqstruct maxlinqmanager \
		maxlinq.help maxlinq.helpfn maxlinq.helpman maxlinq.check maxlinq.isinstance maxlinq.dispose maxlinq.createinstance maxlinq.show \
		maxlinq.nrange maxlinq.srange maxlinq.repeat maxlinq.maxinstances maxlinq.readfile maxlinq.hwnd maxlinq.toarray maxlinq.tolookup maxlinq.toselset

		# Keywords12 - User defined
		style.MAXScript.23=fore:#DA5937,bold
	
	----------------------------------------------
	----------------------------------------------
	February 2017 - V1.00
	----------------------------------------------
	----------------------------------------------
	*/
(	
	global maxLINQ = if ::maxLINQ != undefined do ::maxLINQ
	global maxLINQstruct = if ::maxLINQstruct != undefined do ::maxLINQstruct
	global _From = ::maxLINQstruct

	-------------------------------------------------------------
	-- Struct maxLINQ for Querying Arrays and Strings.
	-------------------------------------------------------------
	if ::maxLINQstruct == undefined do
	(
		struct maxLINQstruct
		(
			public
			ResultArray,
			
			private
			-- Working parameters and functions
			P3D_objMaxLinq = #(),
			vars = "",
			fnExpression = "",
			finalQuery = true,
			subArrayCalc = false,
			indexCalc = false,
			useObject = false,
			useKey = false,
			useDescending = false,
			caseSensitive = true,
			IndexSort = maxLINQ.IndexSort,
			dotNetString = dotNetClass "System.String",
			dotNetStringComparer = dotNetClass "System.StringComparison",
			strCompCI = dotNetStringComparer.OrdinalIgnoreCase,
			LA2,
			ext,
	
			fn initializeModifiers =
			(
				finalQuery = true	
				subArrayCalc = false
				indexCalc = false
				useObject = false
				useKey = false
				useDescending = false
				caseSensitive = true
			),
			fn parser LA =
			(
				LA = (LA as string)		
				assignPos = findString  LA "=>"
				if assignPos == undefined do (messageBox "\nSyntax Error in Lambda Expression.\nThere's no '=>' symbol assignment" title:"SYNTAX ERROR"; fnExpression = ""; return())
				leftPart = toLower (substring LA 1 (assignPos - 1))
				varsPart = filterString leftPart ":"
				numpart = varsPart.count
				
				vars = trimLeft (trimRight varsPart[numpart])
				fnPart = trimLeft (trimRight (substring LA (assignPos+2) -1))
				fnExpression = vars + " = (" + fnPart + ")"
				
				initializeModifiers()
				if numpart > 1 do
				(
					modifs = FilterString varsPart[1] ", \t"
					for mod in modifs do
					(
						case mod of
						(
							"q": finalQuery = false
							"s": subArrayCalc = (isKindOf ResultArray[1] Array) --	SubArrays calculations only valid if ResultArray is a multiArray
							"i": indexCalc = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
				ok
			),
			fn softParser LA =
			(
				LA = (LA as string)
				if (soft = ((substring LA LA.count 1) == ":")) do
				(
					modifs = filterString (toLower LA) ",: \t"
					for mod in modifs do
					(
						case mod of
						(
							"q": finalQuery = false
							"s": subArrayCalc = (isKindOf ResultArray[1] Array)
							"i": indexCalc = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
				return soft
			),
			fn Equals x y result:on = if not result then result else	--	Thanks to DenisT for this Equality of Arrays function
			(
				if (iskindof x bitArray) and (iskindof y bitArray) do return (((x-y).isEmpty and not x.isEmpty) or (x.isEmpty and y.isEmpty))
				if (iskindof x Array) and (iskindof y Array) and x.count == y.count then
				(
					for k=1 to x.count while result do result = Equals x[k] y[k] result:result
					result
				)
				else (x == y)
			),		
			fn intFlatten ar =	
			(
					Result = #()
					for item in ar do (if (isKindOf item Array) then join Result (intFlatten item) else append Result item)
					Result
			),
			fn comparer x y DES:useDescending =
			(
				dir = if DES then -1 else 1
				case of
				(
					(::maxLINQ.LambdaFN x < ::maxLINQ.LambdaFN y): (-dir)
					(::maxLINQ.LambdaFN x > ::maxLINQ.LambdaFN y): dir
					default: 0
				)
			),
			fn caseInsensitiveComparer x y DES:useDescending =
			(
				dir = if DES then -1 else 1
				(stricmp (::maxLINQ.LambdaFN x) (::maxLINQ.LambdaFN y)) * dir
			),
			fn convertToArray =
			(
				case of
				(
					(isKindOf ResultArray Node): ResultArray = join #() ResultArray
					(isKindOf ResultArray bitArray): ResultArray = ResultArray as array
					(isKindOf ResultArray dotNetObject): 	ResultArray = 	(
																			hasItemProp = (try (ResultArray.item[0]; true) catch (false))	--	(enumerable.GetType()).GetProperty "Item" is ambiguous in most cases and throws an error
																			if hasItemProp then
																			(
																				for i = 0 to (ResultArray.count-1) collect (ResultArray.Item[i])
																			)
																			else
																			(
																				isIEnumerable = maxLINQ.IndexSort.isIEnumerable ResultArray
																				if isIEnumerable then
																				(
																					iterator = ResultArray.getEnumerator(); i = 0; prop = #()
																					while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; append prop iterValue)
																					prop
																				)
																				else
																				(
																					#(ResultArray)
																				)
																			)
																		)
				)
				ok
			),
			fn isDotNetString var=
			(
				(isKindOf var dotNetObject) and ((var.getType()).toString() == "System.String")
			),
			
			
			------------------------------------------------------------------------------------------------------
			-- Lambda Expressions Syntax: (valid as names #' ' and strings " ")
			-- (#'[q,s,i,index,obj,key:] var => <var expression>')  : applies <var expression> to each 'var' element of the array
			-- #(extVar, #'[q,s,i,index,obj,key:] var extVar => <var and extVar expression')  : applies <var and extVar expression> to each 'var' element of the array where 'extVar' is an external variable
			-- Modifiers [q,s,i,index,obj,key:]:
				-- Syntax: separated by comas (',') if more than one and ended by colon (':')
				-- q: indicates that it's not the last function in the query. Thus, the returned value will be a struct, not an array. Valid in all Query and Filter functions.
				-- s: works in subarray elements if the array is a multiarray (valid in most query functions)
				-- i: returns indices of elements instead of the elements value (valid in most query functions)
				-- obj: only valid in the  '_Where' and 'Select' functions (and derivatives 'sum', 'count', 'average', '_min' and '_max').Allows to use the variable 'obj' in the lambda expression which holds the maxObject element of a previous query with 'mappedProperty' or 'mappedGeometry'.
				-- key: only valid in the 'mappedProperty', 'mappedGeometry', 'Select' and 'GroupBy' functions. The result query will be an array of 'lookUp' structs with 3 parameters: key, value and count (see each case in their respective help).
				-- d: only valid in the 'Sort' query function. The sort will be done in descending order
				-- ci: The comparisons will be case Insensitive for strings. NOTE: all functions are CaseSensitive by default(valid in some query functions)
			------------------------------------------------------------------------------------------------------
			
			public
			-- QUERY FUNCTIONS (Use a Boolean Lambda Expression. Don't affect to array elements values. Allow further queries)
			fn _Where LA =	--	<ArrayQuery>._Where(<BooleanLambdaExpression var>): Selects the array elements that satisfy the Boolean Lambda Expression.
										--	<ArrayQuery>._Where#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression, which holds the index of each element
										--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
										--	Accepted modifiers: q, s, i, obj
										--
										-- 	'String maxLINQ':
										--	<StringQuery>._Where(<stringToSearch>): Returns an array with the indices of all the matches of the 'stringToSearch' in the original string
										-- 	<StringQuery>._Where#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
										--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; soft = softParser modLA; LA = LA[1])
					
					if LA != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						Result = #()
						case of
						(
							(caseSensitive):
								(
									theLength = LA.count
									found = false; oldIndex = 0; index = 0
									while not found do
									(
										index = stringDN.IndexOf LA oldIndex 
										found = index == -1
										if not found do (append Result (index+1); oldIndex = index + theLength)
									)
								)
							(not caseSensitive):
								(
									theLength = LA.count
									found = false; oldIndex = 0; index = 0
									while not found do
									(
										index = stringDN.IndexOf LA oldIndex strCompCI
										found = index == -1
										if not found do (append Result (index+1); oldIndex = index + theLength)
									)
								)
						)
					)
					return (Result)
				)
	
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if useObject then	
				(
					if not subArrayCalc do P3D_objMaxLinq_temp = #()
					if external then
					(
						LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
						fn LA obj index item = LA2 obj index item ext
					)
					else
					(
						LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
					)
					case of
					(
						(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; ResultArray[id])
						(subArrayCalc and not indexCalc):			ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id]) collect ResultArray[i][id])
						(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; id)	
						(subArrayCalc and indexCalc): 				ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id]) collect id)
					)
					if not subArrayCalc do P3D_objMaxLinq = P3D_objMaxLinq_temp
				)
				else
				(
					if external then
					(
						LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						fn LA index item = LA2 index item ext
					)
					else
					(
						LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					)
					case of
					(
						(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id]) collect ResultArray[id]	--	normal
						(subArrayCalc and not indexCalc):			ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id]) collect Item[id]) -- subarray
						(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id]) collect id	-- index	
						(subArrayCalc and indexCalc): 				ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id]) collect id) -- subarray+index
					)
				)

				return (if finalQuery then ResultArray else this)
			),
			fn W LA = _Where LA,		--	Same as _Where
			fn mappedProperty LA =	--	<ArrayQuery>.mappedProperty(<BooleanLambdaExpression property var>): Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the  Boolean Lambda Expression.
													--	Reserved word 'obj' in lambda expression which holds the original maxObject_element
													--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																				--	Key: holds the MaxObject
																				--	Value: holds the array with the result properties of the query for the key MaxObject
																				--	Count: the number of elements in the array of properties for this MAxObject
													--	Some mapped Properties: verts, faces, edges, selectedVerts, selectedFaces, selectedEdges, children, modifiers, controller.Keys...
													--	Accepted modifiers: q, i, key
													--	Allways returns a multisubarray with each subArray corresponding to the catched properties of each MaxObject in the original array
													--	IMPORTANT NOTE: if you query directly the selected object $ (or its name $Spehere001), without including it in an array (#($) or #($Sphere001))
													--							the mapped property will be taken from the object and all its children and sub-children.
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				ResultArray = maxLINQ.ToArray ResultArray
				P3D_objMaxLinq = deepCopy ResultArray 
				parser LA
				vars = (filterString vars " \t")[1]
				fnExpression = substring fnExpression (vars.count+2) -1
				
				LA =  execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
				case of
				(
					(not indexCalc):	
						(
							ResultArray = for o in ResultArray collect (prop = join #() (getProperty o vars); for p in prop where (LA o p) collect (p))
						)
					(indexCalc):
						(
							ResultArray = for o in ResultArray collect (prop = join #() (getProperty o vars); for p = 1 to prop.count where (LA o prop[p]) collect (p))
						)
				)
				
				if useKey do (ResultArray = for i = 1 to ResultArray.count collect (maxLINQ.toLookUp Key:P3D_objMaxLinq[i] Value:ResultArray[i] Count:ResultArray[i].count))
				
				return (if finalQuery then ResultArray else this)
			),
			fn mappedProp LA = mappedProperty LA,		--	Same as mappedProperty
			fn mappedGeometry LA =	--	<ArrayQuery>.mappedGeometry(<BooleanLambdaExpression GeometyProperty var>): Collects the  'Geometry' of the Geometry_Objects in the array that satisfy the  Boolean Lambda Expression.
												--	Only valid for Verts, Faces and Edges and their 'Selected' ones, for Editable_Meshes and Editable_Polys
												--	Reserved word 'obj' in lambda expression which holds the original maxObject_element
												--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																			--	Key: holds the geometry object
																			--	Value: holds the array with the result of the query for the key object
																			--	Count: the number of elements in the array
												--	Accepted modifiers: q, i, key
												--	Allways returns a multisubarray with each subArray corresponding to the catched geometry of each MaxObject in the original array that is Editable_Mesh or Editable_Poly. Other classes are skiped.
												--	IMPORTANT NOTE: if you query directly the selected object $ (or its name $Spehere001), without including it in an array (#($) or #($Sphere001))
												--							the mapped property will be taken from the object and all its children and sub-children.
												--
												-- 'String maxLINQ':
												-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				P3D_objMaxLinq = #()
				parser LA
				vars = (filterString vars " \t")[1]
				fnExpression = substring fnExpression (vars.count+2) -1
				
				LA =  execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
				case of
				(
					(not indexCalc):		--	normal
						(
							case vars of
							(
								"verts": 				ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for i = 1 to o.numVerts where (LA o (item = getVert o i)) collect item)
																									else (for i = 1 to o.numVerts where (LA o (item = polyop.getVert o i)) collect item))
								"faces": 				ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for i = 1 to o.numFaces where (LA o (item = getFace o i)) collect item)
																									else (for i = 1 to o.numFaces where (LA o (item = polyop.getFaceVerts o i)) collect item))
								"edges": 			ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for ed in o.Edges where (LA o ed) collect ed)
																									else (for i = 1 to (polyop.getnumedges o) where (LA o (item = polyop.getEdgeVerts o i)) collect item))
								"selectedverts": 	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA o (item = getVert o i)) collect item)
																									else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA o (item = polyop.getVert o i)) collect item))
								"selectedfaces":	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA o (item = getFace o i)) collect item)
																									else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA o (item = polyop.getFaceVerts o i)) collect item))
								"selectededges":	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA o (item = o.Edges[i])) collect item)
																									else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA o (item = polyop.getEdgeVerts o i)) collect item))
							)
						)
					(indexCalc):			-- index
						(
							case vars of
							(
								"verts": 				ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for i = 1 to o.numVerts where (LA o (getVert o i)) collect i)
																									else (for i = 1 to o.numVerts where (LA o (polyop.getVert o i)) collect i))
								"faces": 				ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for i = 1 to o.numFaces where (LA o (getFace o i)) collect i)
																									else (for i = 1 to o.numFaces where (LA o (polyop.getFaceVerts o i)) collect i))
								"edges": 			ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (for i = 1 to o.Edges.count where (LA o (o.Edges[i])) collect i)
																									else (for i = 1 to (polyop.getnumedges o) where (LA o (polyop.getEdgeVerts o i)) collect i))
								"selectedverts": 	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA o (getVert o i)) collect i)
																									else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA o (polyop.getVert o i)) collect i))
								"selectedfaces":	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA o (getFace o i)) collect i)
																									else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA o (polyop.getFaceVerts o i)) collect i))
								"selectededges":	ResultArray = for o in ResultArray where ((em = isKindOf o Editable_mesh) or (isKindOf o Editable_Poly)) collect (
														append P3D_objMaxLinq o; if em then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA o (o.Edges[i])) collect i)
																									else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA o (polyop.getEdgeVerts o i)) collect i))
							)
						)
				)
				
				if useKey do (ResultArray = for i = 1 to ResultArray.count collect (maxLINQ.toLookUp Key:P3D_objMaxLinq[i] Value:ResultArray[i] Count:ResultArray[i].count))
				
				return (if finalQuery then ResultArray else this)
			),
			fn mappedGeom LA = mappedGeometry LA,		--	Same as mappedGeometry
			fn DotNetIterator LA =	--	<ArrayQuery>.DotNetIterator(<BooleanLambdaExpression CollectionItem>): Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the  Boolean Lambda Expression.
											--	<ArrayQuery>.DotNetIterator#(<externalVar>, <BooleanLambdaExpression CollectionItem extVar>): same as above using an external variable/maxObject
											--	Reserved word 'index' in lambda expression which holds the index of each element
											--	Some Dotnet Iteratable: List<>, DataGridView.Columns"System.Management.ManagementClass" instances ...
											--	Accepted modifiers: q, s, i
											--
											-- 'String maxLINQ':
											-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
						
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)

				if not (isKindOf ResultArray Array) do ResultArray = #(ResultArray)
				isDotNetObj = if subArrayCalc then (isKindOf ResultArray[1][1] dotNetObject) else (isKindOf ResultArray[1] dotNetObject)
				if not isDotNetObj do return(Format "--	maxLINQ value is not a DotNet Object\n"; undefined)
				
				hasItemProp = if subArrayCalc then (try (ResultArray[1][1].item[0]; true) catch (false)) else (try (ResultArray[1].item[0]; true) catch (false)) 
				isIEnumerable = if not hasItemProp then
				(
					if subArrayCalc then (maxLINQ.IndexSort.isIEnumerable ResultArray[1][1]) else (maxLINQ.IndexSort.isIEnumerable ResultArray[1])
				)
				else (true)
				if not (hasItemProp or isIEnumerable) do  return(Format "--	maxLINQ value is not an IEnumerable DotNet Object\n"; undefined)
				
				if hasItemProp then		--	looping items is faster and less memory than GetEnumerator
				(
					case of
					(
						(not subArrayCalc and not indexCalc):		--	normal 
							(
								ResultArray = for o in ResultArray collect (for i = 0 to (o.count-1) where (LA (i+1) o.Item[i]) collect (o.Item[i]))
							)
						(subArrayCalc and not indexCalc):		--	subArray 			
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (for i = 0 to (o.count-1) where (LA (i+1) o.Item[i]) collect (o.Item[i])))
							)
						(not subArrayCalc and indexCalc):			-- index			
							(
								ResultArray = for o in ResultArray collect (for i = 0 to (o.count-1) where (LA (i+1) o.Item[i]) collect (i+1))
							)
						(subArrayCalc and indexCalc):			-- subarray+index			
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (for i = 0 to (o.count-1) where (LA (i+1) o.Item[i]) collect (i+1)))
							)
					)
				)
				else
				(
					case of
					(
						(not subArrayCalc and not indexCalc):		--	normal 
							(
								ResultArray = for o in ResultArray collect (
									prop = #(); iterator = o.getEnumerator(); i = 0
									while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA i iterValue) do append prop iterValue)
									prop
									)
							)
						(subArrayCalc and not indexCalc):		--	subArray 			
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (
										prop = #(); iterator = o.getEnumerator(); i = 0
										while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA i iterValue) do append prop iterValue)
										prop
										)
								)
							)
						(not subArrayCalc and indexCalc):			-- index			
							(
								ResultArray = for o in ResultArray collect (
									prop = #(); iterator = o.getEnumerator(); i = 0
									while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA i iterValue) do append prop i)
									prop
									)
							)
						(subArrayCalc and indexCalc):			-- subarray+index			
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (
										prop = #(); iterator = o.getEnumerator(); i = 0
										while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA i iterValue) do append prop i)
										prop
										)
								)
							)
					)
				)
				
				if ResultArray.count == 1 and isKindOf ResultArray[1] Array do ResultArray = ResultArray[1]
				return (if finalQuery then ResultArray else this)
			),
			fn DotNetIteratorGet LA =	-- This is not a Query Function (Lambda Expression is not boolean). It's a Filter Function
														--	<ArrayQuery>.DotNetIteratorGet(<LambdaExpression CollectionItem>): Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
														--	<ArrayQuery>.DotNetIteratorGet#(<externalVar>, <LambdaExpression CollectionItem extVar>): same as above using an external variable/maxObject
														--	Reserved word 'index' in lambda expression which holds the index of each element
														--	Some Dotnet Iteratable: List<>, "System.Management.ManagementClass" instances ...
														--	Accepted modifiers: q, s
														--
														-- 'String maxLINQ':
														-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				if not (isKindOf ResultArray Array) do ResultArray = #(ResultArray)
				isDotNetObj = if subArrayCalc then (isKindOf ResultArray[1][1] dotNetObject) else (isKindOf ResultArray[1] dotNetObject)
				if not isDotNetObj do return(Format "--	maxLINQ value is not a DotNet Object\n"; undefined)
				
				hasItemProp = if subArrayCalc then (try (ResultArray[1][1].item[0]; true) catch (false)) else (try (ResultArray[1].item[0]; true) catch (false)) 
				isIEnumerable = if not hasItemProp then
				(
					if subArrayCalc then (maxLINQ.IndexSort.isIEnumerable ResultArray[1][1]) else (maxLINQ.IndexSort.isIEnumerable ResultArray[1])
				)
				else (true)
				if not (hasItemProp or isIEnumerable) do  return(Format "--	maxLINQ value is not an IEnumerable DotNet Object\n"; undefined)
				
				if hasItemProp then		--	looping items is faster and less memory than GetEnumerator
				(
					case of
					(
						(not subArrayCalc):		--	normal 
							(
								ResultArray = for o in ResultArray collect (for i = 0 to (o.count-1) collect (LA (i+1) o.Item[i]))
							)
						(subArrayCalc):		--	subArray 			
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (for i = 0 to (o.count-1) collect (LA (i+1) o.Item[i])))
							)
					)
				)
				else
				(
					case of
					(
						(not subArrayCalc):		--	normal
							(
								ResultArray = for o in ResultArray collect (
									prop = #(); iterator = o.getEnumerator(); i = 0
									while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; append prop (LA i iterValue))
									prop
									)
							)
						(subArrayCalc):		--	subArray 
							(
								ResultArray = for itemMap in ResultArray collect(
									for o in itemMap collect (
										prop = #(); iterator = o.getEnumerator(); i = 0
										while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; append prop (LA i iterValue))
										prop
										)
								)
							)
					)
				)
				
				if ResultArray.count == 1 and isKindOf ResultArray[1] Array do ResultArray = ResultArray[1]
				return (if finalQuery then ResultArray else this)
			),
			fn TakeWhile LA =	--	<ArrayQuery>.TakeWhile(<BooleanLambdaExpression var>): Returns the array elements as long as the Boolean Lambda Expression is true
											--	<ArrayQuery>.TakeWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
											--	Reserved word 'index' in lambda expression which holds the index of each element
											--	Accepted modifiers: q, s, i
											--
											-- 	'String maxLINQ':
											--	<StringQuery>.TakeWhile(<stringToSearch>): Returns the string before the first occurrence of 'stringToSearch'
											-- 	<StringQuery>.TakeWhile#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
											--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; soft = softParser modLA; LA = LA[1])
					
					if LA != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						index = if caseSensitive then (stringDN.IndexOf LA) else (stringDN.IndexOf LA strCompCI)
						--if index == -1 do index = 0
						ResultArray = subString ResultArray 1 index
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				ResultArray = 
					case of
					(
						(not subArrayCalc and not indexCalc):	for id = 1 to ResultArray.count while (LA id ResultArray[id]) collect ResultArray[id]	--	normal
						(subArrayCalc and not indexCalc):			for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id])  collect Item[id]) -- subarray
						(not subArrayCalc and indexCalc):			for id = 1 to ResultArray.count while (LA id ResultArray[id]) collect id	-- index	
						(subArrayCalc and indexCalc): 				for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id]) collect id) -- subarray+index
					)
				
				return (if finalQuery then ResultArray else this)
			),
			fn SkipWhile LA =	--	<ArrayQuery>.SkipWhile(<BooleanLambdaExpression var>): Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
											--	<ArrayQuery>.SkipWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
											--	Reserved word 'index' in lambda expression which holds the index of each element
											--	Accepted modifiers: q, s, i
											--
											-- 	'String maxLINQ':
											--	<StringQuery>.SkipWhile(<stringToSearch>): Returns the string after the first occurrence of 'stringToSearch' ('stringToSearch' included)
											-- 	<StringQuery>.SkipWhile#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
											--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; soft = softParser modLA; LA = LA[1])
					
					if LA != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						index = if caseSensitive then (stringDN.IndexOf LA) else (stringDN.IndexOf LA strCompCI)
						if index != -1 then ResultArray = subString ResultArray (index+1) -1 else ResultArray = ""
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				ResultArray = 
					case of
					(
						(not subArrayCalc and not indexCalc):	(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect ResultArray[id])	--	normal
						(subArrayCalc and not indexCalc):			for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect Item[id]) -- subarray
						(not subArrayCalc and indexCalc):			(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect id)	-- index	
						(subArrayCalc and indexCalc): 				for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect id) -- subarray+index
					)
				
				return (if finalQuery then ResultArray else this)
			),
			
			-- FILTER FUNCTIONS (Most use a Lambda Expression for projecting array values. Array values change for 'Select'=='Apply' functions. Allow further queries)
			fn Select LA =	--	<ArrayQuery>.Select(<LambdaExpression var>): Creates a new array from input array by applying Lambda Expression to array elements (the result array has the same number of elements) . Changes array values.
									--	<ArrayQuery>.Select#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Reserved word 'index' in lambda expression which holds the index of each element
									--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
									--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																--	Key: holds the original array element value
																--	Value: holds the projected value after applying the Lambda Expression to the array element
																--	Count: the number of elements the projected value
									--	Accepted modifiers: q, s, obj, key
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String maxLINQ'. Use 'Replace' or 'ReplaceAt' functions to modify the string
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if useKey then (ResultArray = maxLINQ.ToArray ResultArray; oldResultArray = deepCopy ResultArray) else convertToArray()
				
				if useObject then	
				(
					if external then
					(
						LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
						fn LA obj index item = LA2 obj index item ext
					)
					else
					(
						LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
					)
					
					case of
					(
						(not subArrayCalc):	ResultArray = for i = 1 to ResultArray.count collect (LA P3D_objMaxLinq[i] i ResultArray[i])	
						(subArrayCalc):		ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count collect (LA P3D_objMaxLinq[i] id ResultArray[i][id]))
					)
				)
				else
				(
					if external then
					(
						LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						fn LA index item = LA2 index item ext
					)
					else
					(
						LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					)
					
					case of
					(
						(not subArrayCalc):	ResultArray = for id = 1 to ResultArray.count collect (LA id ResultArray[id])	--	normal
						(subArrayCalc):		ResultArray = for item in ResultArray collect (for id = 1 to item.count collect (LA id Item[id])) -- subarray
					)
				)
				
				if useKey do (ResultArray = for i = 1 to ResultArray.count collect (maxLINQ.toLookUp Key:oldResultArray[i] Value:ResultArray[i]))
				
				return (if finalQuery then ResultArray else this)
			),
			fn Apply LA = Select LA,	--	Same as Select
			fn GroupBy LA =	--	<ArrayQuery>.GroupBy(<LambdaExpression var>): Creates a new array from input array by grouping the array elements that evaluate to the same value when applying to them the Lambda Expression
										--	Uses 'Default Equality' function for comparing arrays (if the result of the Lambda Expression is an array)
										--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																	--	Key: the key value of all elements (the value that all group members evaluate to)
																	--	Value: the array with the elements that evaluate to the Key property
																	--	Count: the number of elements in the group
										--	Accepted modifiers: q, s, i, key
										--
										-- 'String maxLINQ':
										-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				parser LA
				LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression)

				if useKey then
				(
					case of
					(
						(not subArrayCalc and not indexCalc):	--	normal
							(
								tempResult = #()
								notFound = #{1..ResultArray.count}
								for i = 1 to ResultArray.count where notFound[i] do		
								(
									ResultStruct = maxLINQ.toLookUp()
									ResultStruct.Value = #()
									append ResultStruct.Value ResultArray[i]
									ResultStruct.Key = checkValue = LA ResultArray[i]
									
									for j = i+1 to ResultArray.count where (notFound[j] and (Equals (LA ResultArray[j]) checkValue)) do
									(
										notFound[j] = false
										append ResultStruct.Value ResultArray[j]
									)
									ResultStruct.Count = ResultStruct.Value.Count
									append tempResult ResultStruct
								)

								ResultArray = tempResult
							)
						(subArrayCalc and not indexCalc):			-- subarray
							(
								ResultArray = for item in ResultArray collect
								(
									tempResult = #()
									notFound = #{1..item.count}
									for i = 1 to item.count where notFound[i] do		
									(
										ResultStruct = maxLINQ.toLookUp()
										ResultStruct.Value = #()
										append ResultStruct.Value item[i]
										ResultStruct.Key = checkValue = LA item[i]
										
										for j = i+1 to item.count where (notFound[j] and (Equals (LA item[j]) checkValue)) do
										(
											notFound[j] = false
											append ResultStruct.Value item[j]
										)
									ResultStruct.Count = ResultStruct.Value.Count
									append tempResult ResultStruct
									)
									tempResult
								)
							)
						(not subArrayCalc and indexCalc):			-- index	
							(
								tempResult = #()
								notFound = #{1..ResultArray.count}
								for i = 1 to ResultArray.count where notFound[i] do		
								(
									ResultStruct = maxLINQ.toLookUp()
									ResultStruct.Value = #()
									append ResultStruct.Value i
									ResultStruct.Key = checkValue = LA ResultArray[i]
									
									for j = i+1 to ResultArray.count where (notFound[j] and (Equals (LA ResultArray[j]) checkValue)) do
									(
										notFound[j] = false
										append ResultStruct.Value j
									)
									ResultStruct.Count = ResultStruct.Value.Count
									append tempResult ResultStruct
								)

								ResultArray = tempResult
							)
						(subArrayCalc and indexCalc): 				-- subarray+index
							(
								ResultArray = for item in ResultArray collect
								(
									tempResult = #()
									notFound = #{1..item.count}
									for i = 1 to item.count where notFound[i] do		
									(
										ResultStruct = maxLINQ.toLookUp()
										ResultStruct.Value = #()
										append ResultStruct.Value i
										ResultStruct.Key = checkValue = LA item[i]
										
										for j = i+1 to item.count where (notFound[j] and (Equals (LA item[j]) checkValue)) do
										(
											notFound[j] = false
											append ResultStruct.Value j
										)
									ResultStruct.Count = ResultStruct.Value.Count
									append tempResult ResultStruct
									)
									tempResult
								)
							)
					)
				)
				else
				(
					case of
					(
						(not subArrayCalc and not indexCalc):	--	normal
							(
								tempResult = #()
								notFound = #{1..ResultArray.count}
								count = 0
								for i = 1 to ResultArray.count where notFound[i] do		
								(
									count += 1
									append tempResult #()
									append tempResult[count] ResultArray[i]
									checkValue = LA ResultArray[i]
									
									for j = i+1 to ResultArray.count where (notFound[j] and (Equals (LA ResultArray[j]) checkValue)) do
									(
										notFound[j] = false
										append tempResult[count] ResultArray[j]
									)
								)

								ResultArray = tempResult
							)
						(subArrayCalc and not indexCalc):			-- subarray
							(
								ResultArray = for item in ResultArray collect
								(
									tempResult = #()
									notFound = #{1..item.count}
									count = 0
									for i = 1 to item.count where notFound[i] do		
									(
										count += 1
										append tempResult #()
										append tempResult[count] item[i]
										checkValue = LA item[i]
										
										for j = i+1 to item.count where (notFound[j] and (Equals (LA item[j]) checkValue)) do
										(
											notFound[j] = false
											append tempResult[count] item[j]
										)
									)
									tempResult
								)
							)
						(not subArrayCalc and indexCalc):			-- index	
							(
								tempResult = #()
								notFound = #{1..ResultArray.count}
								count = 0
								for i = 1 to ResultArray.count where notFound[i] do		
								(
									count += 1
									append tempResult #()
									append tempResult[count] i
									checkValue = LA ResultArray[i]
									
									for j = i+1 to ResultArray.count where (notFound[j] and (Equals (LA ResultArray[j]) checkValue)) do
									(
										notFound[j] = false
										append tempResult[count] j
									)
								)

								ResultArray = tempResult
							)
						(subArrayCalc and indexCalc): 				-- subarray+index
							(
								ResultArray = for item in ResultArray collect
								(
									tempResult = #()
									notFound = #{1..item.count}
									count = 0
									for i = 1 to item.count where notFound[i] do		
									(
										count += 1
										append tempResult #()
										append tempResult[count] i
										checkValue = LA item[i]
										
										for j = i+1 to item.count where (notFound[j] and (Equals (LA item[j]) checkValue)) do
										(
											notFound[j] = false
											append tempResult[count] j
										)
									)
									tempResult
								)
							)
					)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn MakeUnique LA =		--	<ArrayQuery>.MakeUnique#(): Creates a new array with unique elements. Uses 'Default Equality' function for comparing arrays
													--	<ArrayQuery>.MakeUnique(<LambdaExpression>): compares array elements after applying the Lambda expression to each element of the array
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
					
				case of
				(
					(not subArrayCalc and not indexCalc):		--	normal
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								found = #{1..ResultArray.count}
								ResultArray = for i = 1 to ResultArray.count where found[i] collect
									(
										for j = i+1 to ResultArray.count where found[j] do
										(
											if useLA 	then (if Equals (LA ResultArray[i]) (LA ResultArray[j]) do found[j] = false) 
															else (if Equals ResultArray[i] ResultArray[j] do found[j] = false)
										)
										ResultArray[i]
									)
							)
							else
							(
								ResultArray = makeUniqueArray ResultArray
							)
						)
					(subArrayCalc and not indexCalc):		-- subarray
						(
							ResultArray = for item in ResultArray collect
							(
								if (iskindof item[1] Array) or (useLA) then
								(
									found = #{1..item.count}
									Result = for i = 1 to item.count where found[i] collect
										(
											for j = i+1 to item.count where found[j] do
											(
												if useLA 	then (if Equals (LA item[i]) (LA item[j]) do found[j] = false) 
																else (if Equals item[i] item[j] do found[j] = false)
											)
											item[i]
										)
								)
								else
								(
									Result = makeUniqueArray item
								)
							)
						)
					(not subArrayCalc and indexCalc):		--	index
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								found = #{1..ResultArray.count}
								ResultArray = for i = 1 to ResultArray.count where found[i] collect
									(
										for j = i+1 to ResultArray.count where found[j] do
										(
											if useLA 	then (if Equals (LA ResultArray[i]) (LA ResultArray[j]) do found[j] = false) 
															else (if Equals ResultArray[i] ResultArray[j] do found[j] = false)
										)
										i
									)
							)
							else
							(
								found = #{1..ResultArray.count}
								ResultArray = for i = 1 to ResultArray.count where found[i] collect
									(
										for j = i+1 to ResultArray.count where found[j] do
										(
											if ResultArray[i] == ResultArray[j] do found[j] = false
										)
										i
									)
							)
						)
					(subArrayCalc and indexCalc):		-- subarray+index
						(
							ResultArray = for item in ResultArray collect
							(
								if (iskindof item[1] Array) or (useLA) then
								(
									found = #{1..item.count}
									Result = for i = 1 to item.count where found[i] collect
										(
											for j = i+1 to item.count where found[j] do
											(
												if useLA 	then (if Equals (LA item[i]) (LA item[j]) do found[j] = false) 
																else (if Equals item[i] item[j] do found[j] = false)
											)
											i
										)
								)
								else
								(
									found = #{1..item.count}
									Result = for i = 1 to item.count where found[i] collect
										(
											for j = i+1 to item.count where found[j] do
											(
												if item[i] == item[j] do found[j] = false
											)
											i
										)
								)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Distinct LA = MakeUnique LA,	--	Same as MakeUnique
			fn Intersect otherArray =	--	<ArrayQuery>.Intersect(otherArray): Creates a new array with the common elements of both arrays. Uses 'Default Equality' function for comparing arrays.
														--	<ArrayQuery>.Intersect#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
														--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
														--	This function doesn't "MakeUnique" result. If one item in the original array is duplicated and it exists in 'otherArray', it will be catched twice.
														--	Accepted modifiers: q, s, i
														--
														-- 'String maxLINQ':
														-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				useLA = false
				initializeModifiers()

				if (otherArray.count == 2) and (not (isKindOf otherArray[1] name) and not (isKindOf otherArray[1] string)) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) then
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
				otherArray = maxLinq.ToArray otherArray
				if subArrayCalc do (if ResultArray.count != otherArray.count do return(Format "--	Error: for subArray calculations, both arrays must have the same number of items\n"; undefined))
					
				case of
				(
					(not subArrayCalc and not indexCalc):		--	normal
					
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								ResultArray = for item in ResultArray where
													(
														notfound = true
														for item2 in otherArray while notfound do
														(
															if useLA 	then (if Equals (LA item) (LA item2) do (notfound = false)) 
																			else (if Equals item item2 do (notfound = false))
														)
														not notfound
													)
													collect item
							)
							else
							(
								ResultArray = for item in ResultArray where (findItem otherArray item != 0) collect item
							)
						)
					(subArrayCalc and not indexCalc):		-- subarray
						(
							ResultArray = for i = 1 to ResultArray.count collect
							(
								if (iskindof ResultArray[i][1] Array) or (useLA) then
								(
									Result = for item in ResultArray[i] where
													(
														notfound = true
														for item2 in otherArray[i] while notfound do
														(
															if useLA 	then (if Equals (LA item) (LA item2) do (notfound = false)) 
																			else (if Equals item item2 do (notfound = false))
														)
														not notfound
													)
													collect item
								)
								else
								(
									Result = for item in ResultArray[i] where (findItem otherArray[i] item != 0) collect item
								)
							)
						)
					(not subArrayCalc and indexCalc):		--	index
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								ResultArray = for i = 1 to ResultArray.count where
													(
														notfound = true
														for item2 in otherArray while notfound do
														(
															if useLA 	then (if Equals (LA ResultArray[i]) (LA item2) do (notfound = false)) 
																			else (if Equals ResultArray[i] item2 do (notfound = false))
														)
														not notfound
													)
													collect i
							)
							else
							(
								ResultArray = for i = 1 to ResultArray.count where (findItem otherArray ResultArray[i] != 0) collect i
							)
						)
					(subArrayCalc and indexCalc):		-- subarray+index
						(
							ResultArray = for i = 1 to ResultArray.count collect
							(
								if (iskindof ResultArray[i][1] Array) or (useLA) then
								(
									Result = for j = 1 to ResultArray[i].count where
													(
														notfound = true
														for item2 in otherArray[i] while notfound do
														(
															if useLA 	then (if Equals (LA ResultArray[i][j]) (LA item2) do (notfound = false)) 
																			else (if Equals ResultArray[i][j] item2 do (notfound = false))
														)
														not notfound
													)
													collect j
								)
								else
								(
									Result = for j = 1 to ResultArray[i].count where (findItem otherArray[i] ResultArray[i][j] != 0) collect j
								)
							)
						)
				)
				
				
				return (if finalQuery then ResultArray else this)
			),
			fn Except otherArray =	--	<ArrayQuery>.Except(otherArray): Creates a new array with the elements of the original array that are not in the 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
													--	<ArrayQuery>.Except#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	This function doesn't "MakeUnique" result. If one item in the original array is duplicated and it doesn't exist in 'otherArray', it will be catched twice
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				useLA = false
				initializeModifiers()

				if (otherArray.count == 2) and (not (isKindOf otherArray[1] name) and not (isKindOf otherArray[1] string)) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
					
				otherArray = maxLinq.ToArray otherArray
				if subArrayCalc do (if ResultArray.count != otherArray.count do return(Format "--	Error: for subArray calculations, both arrays must have the same number of items\n"; undefined))
					
				case of
				(
					(not subArrayCalc and not indexCalc):		--	normal
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								ResultArray = for item in ResultArray where
													(
														notfound = true
														for item2 in otherArray while notfound do
														(
															if useLA 	then (if Equals (LA item) (LA item2) do (notfound = false)) 
																			else (if Equals item item2 do (notfound = false))
														)
														notfound
													)
													collect item
							)
							else
							(
								ResultArray = for item in ResultArray where (findItem otherArray item == 0) collect item 
							)
						)
					(subArrayCalc and not indexCalc):		-- subarray
						(
							ResultArray = for i = 1 to ResultArray.count collect
							(
								if (iskindof ResultArray[i][1] Array) or (useLA) then
								(
									Result = for item in ResultArray[i] where
													(
														notfound = true
														for item2 in otherArray[i] while notfound do
														(
															if useLA 	then (if Equals (LA item) (LA item2) do (notfound = false)) 
																			else (if Equals item item2 do (notfound = false))
														)
														notfound
													)
													collect item
								)
								else
								(
									Result = for item in ResultArray[i] where (findItem otherArray[i] item == 0) collect item
								)
							)
						)
					(not subArrayCalc and indexCalc):		--	index
						(
							if (iskindof ResultArray[1] Array) or (useLA) then
							(
								ResultArray = for i = 1 to ResultArray.count where
													(
														notfound = true
														for item2 in otherArray while notfound do
														(
															if useLA 	then (if Equals (LA ResultArray[i]) (LA item2) do (notfound = false)) 
																			else (if Equals ResultArray[i] item2 do (notfound = false))
														)
														notfound
													)
													collect i
							)
							else
							(
								ResultArray = for i = 1 to ResultArray.count where (findItem otherArray ResultArray[i] == 0) collect i 
							)
						)
					(subArrayCalc and indexCalc):		-- subarray+index
						(
							ResultArray = for i = 1 to ResultArray.count collect
							(
								if (iskindof ResultArray[i][1] Array) or (useLA) then
								(
									Result = for j = 1 to ResultArray[i] where
													(
														notfound = true
														for item2 in otherArray[i] while notfound do
														(
															if useLA 	then (if Equals (LA ResultArray[i][j]) (LA item2) do (notfound = false)) 
																			else (if Equals ResultArray[i][j] item2 do (notfound = false))
														)
														notfound
													)
													collect j
								)
								else
								(
									Result = for j = 1 to ResultArray[i].count where (findItem otherArray[i] ResultArray[i][j] == 0) collect j
								)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Union otherArray =		--	<ArrayQuery>.Union(otherArray): Creates a new array with all unique elements of original array and 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
													--	<ArrayQuery>.Union#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	This function does a "MakeUnique" result. Duplicated elements will desappear. When returning indices, the ones from the second array start at: index = (original array).count +1
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				ResultArray = maxLINQ.ToArray ResultArray
				subArrayCalc = false

				if (otherArray.count == 2) and (not (isKindOf otherArray[1] name) and not (isKindOf otherArray[1] string)) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) then
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (parser LA)
				)
				else
				(
					LA = #()
				)
				
				
				if subArrayCalc then
				(
					ResultArray = for i = 1 to ResultArray.Count collect (ResultArray[i] + otherArray[i])
				)
				else
				(
					ResultArray = (ResultArray + otherArray)
				)
				
				MakeUnique LA	
				
				return (if finalQuery then ResultArray else this)
			),
			fn Concat otherArray =	--	<ArrayQuery>.Concat(otherArray): Creates a new array concatenating all elements of original array and 'otherArray'. 
													--	<ArrayQuery>.Concat#(otherArray, <LambdaExpression var>): concatenates both arrays after applying the Lambda expression to each element of both arrays. 
													--	Reserved Lambda expressions ("s:") - with optional 'q' modifier - for fast calculation in case of subArray modifier without a Lambda Expression Value
													--	This function doesn't "MakeUnique" result.
													--	Accepted modifiers: q, s
													--
													-- 	'String maxLINQ':
													--	<StringQuery>.Concat(<stringToAppend>): Appends 'stringToAppend' to the original string
													--	Accepted modifiers: q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf otherArray Array) do (LA = otherArray[2]; otherArray = otherArray[1]; soft = softParser LA)
					ResultArray += otherArray
					return (if finalQuery then ResultArray else this)
				)

				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()

				if (otherArray.count == 2) and (not (isKindOf otherArray[1] name) and not (isKindOf otherArray[1] string)) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) then
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (Select LA; useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
				if subArrayCalc then
				(
					if ResultArray.count != otherArray.count or not isKindOf otherArray[1] array or not isKindOf ResultArray[1] array do return(Format "--	maxLINQ ERROR. Both arrays must have the same dimension\n"; undefined)
					if useLA do otherArray = for item in otherArray collect for subItem in item collect (LA subItem)
					ResultArray = for i = 1 to ResultArray.Count collect (ResultArray[i] + otherArray[i])
				)
				else
				(
					if useLA do otherArray = for item in otherArray collect (LA item)
					ResultArray = (ResultArray + otherArray)
				)
				
				
				return (if finalQuery then ResultArray else this)
			),
			fn flatten LA =	--	<ArrayQuery>.Flatten#(): Flattens one level of the array
									--	<ArrayQuery>.Flatten("q:"): same than above but not final result
									--	Accepted modifiers: q
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
				)
				
				if isKindOf ResultArray[1] Array do
				(
					Result = #()
					for item in ResultArray do join Result item
					ResultArray = Result
				)
				return (if finalQuery then ResultArray else this)
			),
			fn flattenAll LA =	--	<ArrayQuery>.flattenAll#(): Flattens all levels of the array. The result array is a one dimension array.
										--	<ArrayQuery>.flattenAll("q:"): same than above but not final result
										--	Accepted modifiers: q
										--
										-- 'String maxLINQ':
										-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
				)
				
				ResultArray = intFlatten ResultArray
				return (if finalQuery then ResultArray else this)
			),
			fn Sort LA =		--	<ArrayQuery>.Sort#(): Returns the sorted array. All the elements must be comparable
									--	<ArrayQuery>.Sort("s:"): Returns a multiarray with the sorted subarrays(valid in combination other modifiers). All the elements in subArrays must be comparable
									--	<ArrayQuery>.Sort("i:"): Returns an array with the sorted index of the original array (valid in combination other modifiers). All the elements must be comparable
									--	<ArrayQuery>.Sort("d:"): Returns the sorted array in descending order (valid in combination other modifiers). All the elements must be comparable
									--	<ArrayQuery>.Sort("ci:"): The sort will be caseInsensitive for strings (valid in combination other modifiers). All the elements must be comparable
									--	<ArrayQuery>.Sort(<LambdaExpression var>): Returns the sorted array with the comparing criteria of the item value after applying the Lambda Expression(*). Doesn't change array values.
																-- (*) Lambda Expression must evaluate to a comparable value. Valid in combination with 's', 'i', 'd' and 'ci' modifiers.
									--	Accepted modifiers: q, s, i, d, ci
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
				if useLA then
				(
					case of
					(
						(not subArrayCalc and not indexCalc):		--	normal
							(
								if caseSensitive then
								(
									tempResult = deepCopy ResultArray
									qsort tempResult comparer
									ResultArray = tempResult
								)
								else
								(
									tempResult = deepCopy ResultArray
									if isKindOf (LA tempResult[1]) string then (qsort tempResult caseInsensitiveComparer) else (qsort tempResult comparer)
									ResultArray = tempResult
								)
							)
						(subArrayCalc and not indexCalc):		--	subArray
							(
								if caseSensitive then
								(
									ResultArray = for item in ResultArray collect (qsort item comparer; item)
								)
								else
								(
									ResultArray = for item in ResultArray collect 
										(
											if isKindOf (LA item[1]) string then (qsort item caseInsensitiveComparer; item) else (qsort item comparer; item)
										)
								)
								
							)
						(not subArrayCalc and indexCalc):		--	index
							(
								tempResult = for item in ResultArray collect (LA item)
								case (classof tempResult[1]) of
								(
									Integer:	(IndexSort.qSortInt  tempResult useDescending; ResultArray = IndexSort.indices)
									Float: 	(IndexSort.qSortFloat  tempResult useDescending; ResultArray = IndexSort.indices)
									String: 	(IndexSort.qSortString  tempResult useDescending caseSensitive; ResultArray = IndexSort.indices)
								)
							)
						(subArrayCalc and indexCalc):		--	subArray + index
							(
								ResultArray = for item in ResultArray collect 
									(
										tempResult = for subItem in item collect (LA subItem)
										case (classof tempResult[1]) of
										(
											Integer:	(IndexSort.qSortInt  tempResult useDescending; IndexSort.indices)
											Float: 	(IndexSort.qSortFloat  tempResult useDescending; IndexSort.indices)
											String: 	(IndexSort.qSortString  tempResult useDescending caseSensitive; IndexSort.indices)
										)
									)
							)
					)
				)
				else
				(
					case of
					(
						(not subArrayCalc and not indexCalc):		--	normal
							(
								case (classof ResultArray[1]) of
								(
									Integer:	ResultArray = IndexSort.qSortInt  ResultArray useDescending
									Float: 	ResultArray = IndexSort.qSortFloat  ResultArray useDescending
									String: 	ResultArray = IndexSort.qSortString  ResultArray useDescending caseSensitive
								)
							)
						(subArrayCalc and not indexCalc):		--	subArray
							(
								ResultArray = for item in ResultArray collect 
									(
										case (classof item[1]) of
										(
											Integer:	IndexSort.qSortInt  item useDescending
											Float: 	IndexSort.qSortFloat  item useDescending
											String: 	IndexSort.qSortString  item useDescending caseSensitive
										)
									)
							)
						(not subArrayCalc and indexCalc):		--	index
							(
								case (classof ResultArray[1]) of
								(
									Integer:	(IndexSort.qSortInt  ResultArray useDescending; ResultArray = IndexSort.indices)
									Float: 	(IndexSort.qSortFloat  ResultArray useDescending; ResultArray = IndexSort.indices)
									String: 	(IndexSort.qSortString  ResultArray useDescending caseSensitive; ResultArray = IndexSort.indices)
								)
							)
						(subArrayCalc and indexCalc):		--	subArray + index
							(
								ResultArray = for item in ResultArray collect 
									(
										case (classof item[1]) of
										(
											Integer:	(IndexSort.qSortInt  item useDescending; IndexSort.indices)
											Float: 	(IndexSort.qSortFloat  item useDescending; IndexSort.indices)
											String: 	(IndexSort.qSortString  item useDescending caseSensitive; IndexSort.indices)
										)
									)
							)
					)
				)

				return (if finalQuery then ResultArray else this)
			),
			fn orderBy LA = Sort LA,		--	Same as Sort
			fn ReOrder indexArray =	--	<ArrayQuery>.ReOrder(indexArray): Returns the array reordered based on the arrayIndex order. 
												--	<ArrayQuery>.ReOrder#(indexArray, "s:"): Returns a multiarray with the reordered subarrays based on the arrayIndex subarrays order. 
												--	indexArray must be an integer array (or multiarray) of the same length than the array to sort, one-based index.
												--	The n'th index value in the 'indexArray' determines that the 'index value' element in the original array must be in the n'th position in the result array.
												--	Accepted modifiers: q, s
												--
												-- 'String maxLINQ':
												-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				convertToArray()
				initializeModifiers()
				
				if (indexArray.count == 2) and (isKindOf indexArray[1] Array) and ((isKindOf indexArray[2] name) or (isKindOf indexArray[2] string)) do
				(
					LA = indexArray[2]
					indexArray = indexArray[1]
					soft = softParser LA
				)
				
				if subArrayCalc do (if ResultArray.count != indexArray.count do return(Format "--	Error: for subArray calculations, both arrays must have the same number of items\n"; undefined))
				
				case of
				(
					(not subArrayCalc):	
						(
							numItems = ResultArray.count
							ResultArray = for i in indexArray where i <= numItems collect ResultArray[i]
						)
					(subArrayCalc):	
						(
							ResultArray = for i = 1 to ResultArray.count collect 
								(
									numItems = ResultArray[i].count
									for j in indexArray[i] where j <= numItems collect ResultArray[i][j]
								)
						)
				)

				return (if finalQuery then ResultArray else this)
			),
			fn Reverse LA = 	--	<ArrayQuery>.Reverse#(): Reverses the order of the array
									--	<ArrayQuery>.Reverse("s:"): Reverses the order of the sub_arrays of the multiarray
									--	Accepted modifiers: q, s
									--
									-- 	'String maxLINQ':
									--	<StringQuery>.Reverse#(): Reverses the order of the original string
									--	<StringQuery>.Reverse("q:"): Same as above but allows further queries
									--	Accepted modifiers: q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if not (Equals LA #()) do
					(
						soft = softParser LA
					)
					Result =""
					for i = ResultArray.count to 1 by -1 do Result += ResultArray[i]
					ResultArray = Result

					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
				)
					
				ResultArray = 	case of
					(
						(not subArrayCalc): (for i = ResultArray.count to 1 by -1 collect ResultArray[i])	
						(subArrayCalc):		(for item in ResultArray collect (for i = item.count to 1 by -1 collect item[i]))
					)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Take int32 =	-- <ArrayQuery>.Take(int32): Returns the specified number of contiguous elements 'int32' from the start of the array. Int32 is an integer value
										-- <ArrayQuery>.Take(int32Array): Returns the specified number of contiguous elements in 'int32Array' from the start of each subarray in the array. Int32Array is an array of integers. 's:' modifier is set by default.
										-- <ArrayQuery>.Take#(int32Array, "s:"): same as above
										-- <ArrayQuery>.Take#(int32, "s:"): Returns the specified number of contiguous elements 'int32' from the start of each subarray in the array. Int32 is an integer value
										--	Accepted modifiers: q, s
										--
										-- 	'String maxLINQ':
										--	<StringQuery>.Take(int32): Returns the string truncated to its int32 first characters
										-- 	<StringQuery>.Take#(int32, "q:"): same as above but allows a next String Query
										--	Accepted modifiers: q
			(
				initializeModifiers()
				
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					if (isKindOf int32 Array) do
					(
						LA = int32[2]
						int32 = int32[1]
						soft = softParser LA
					)
					ResultArray = substring ResultArray 1 int32
					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				if (isKindOf int32 Array) do
				(
					if ((isKindOf int32[2] name) or (isKindOf int32[2] string)) do
					(
						LA = int32[2]
						int32 = int32[1]
						soft = softParser LA
					)
				)
				if (isKindOf int32 Array) do subArrayCalc = true
				
				ResultArray = 	case of
					(
						(not subArrayCalc): (for i = 1 to int32 while i <= ResultArray.count collect ResultArray[i])	
						(subArrayCalc):		(if (isKindOf int32 Array) then
															(for i = 1 to ResultArray.count collect (for j = 1 to int32[i] while j <= ResultArray[i].count collect ResultArray[i][j]))
															else
															(for i = 1 to ResultArray.count collect (for j = 1 to int32 while j <= ResultArray[i].count collect ResultArray[i][j]))
													)
					)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Skip int32 =	-- <ArrayQuery>.Skip(int32): Bypasses the specified number of contiguous elements 'int32' from the start of the array and then returns the remaining elements. Int32 is an integer value
									-- <ArrayQuery>.Skip(int32Array): same as above for each subarray in the array. Int32Array is an array of integers. 's:' modifier is set by default.
									-- <ArrayQuery>.Skip#(int32Array, "s:"): same as above
									-- <ArrayQuery>.Skip#(int32, "s:"): Bypasses the specified number of contiguous elements 'int32' from the start of each subarray in the array and then returns the remaining elements. Int32 is an integer value
									--	Accepted modifiers: q, s
									--
									-- 	'String maxLINQ':
									--	<StringQuery>.Skip(int32): Returns the string without its int32 first characters
									-- 	<StringQuery>.Skip#(int32, "q:"): same as above but allows a next String Query
									--	Accepted modifiers: q
			(
				initializeModifiers()
				
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					if (isKindOf int32 Array) do
					(
						LA = int32[2]
						int32 = int32[1]
						soft = softParser LA
					)
					ResultArray = substring ResultArray (int32+1) -1
					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				if (isKindOf int32 Array) do
				(
					if ((isKindOf int32[2] name) or (isKindOf int32[2] string)) do
					(
						LA = int32[2]
						int32 = int32[1]
						soft = softParser LA
					)
				)
				if (isKindOf int32 Array) do subArrayCalc = true
				
				ResultArray = 	case of
					(
						(not subArrayCalc): (for i = int32+1 to ResultArray.count collect ResultArray[i])	
						(subArrayCalc):		(if (isKindOf int32 Array) then
															(for i = 1 to ResultArray.count collect (for j = int32[i]+1 to ResultArray[i].count collect ResultArray[i][j]))
															else
															(for i = 1 to ResultArray.count collect (for j = int32+1 to ResultArray[i].count collect ResultArray[i][j]))
													)
					)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Replace valuePairArray =	--	<ArrayQuery>.Replace#(someValue, otherValue): Replaces all occurrences of 'someValue' in the array with 'otherValue'.
													--	<ArrayQuery>.Replace#(someValue, otherValue, "s:"): Same as above but searchs 'someValue' in the subarray elements. 
													--	<ArrayQuery>.Replace#(someValue, otherValue, "ci:"): forces a case Insensitive comparison for strings
													--	<ArrayQuery>.Replace#(someValue, otherValue, <LambdaExpression var>):  Determines the equality to 'someValue' of the array elements after apllying them the Lambda Expression (i.e., items that evaluate to 'someValue').
													--	Accepted modifiers: s, ci, q
													--	'someValue' and 'otherValue' can be an array. In this case, default array equality is used (allways case sensitive)
													--
													-- 	'String maxLINQ':
													--	<StringQuery>.Replace#(<stringToReplace>, <ReplacingString>):Replaces all occurrences of 'stringToReplace' with 'ReplacingString'
													-- 	<StringQuery>.Replace#(<stringToReplace>, <ReplacingString>, "ci:"): same as above using case insensitive comparison
													--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (valuePairArray.count == 3) do (LA = valuePairArray[3]; soft = softParser LA)
					someValue = valuePairArray[1]
					otherValue = valuePairArray[2]
					if someValue != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						ResultArray = case of
						(
							(caseSensitive):
								(
									stringDN.replace someValue otherValue
								)
							(not caseSensitive):
								(
									theLength = someValue.count
									found = false; result = ""; oldIndex = 0; index = 0
									while not found do
									(
										index = stringDN.IndexOf someValue oldIndex strCompCI
										found = index == -1
										if not found do (result +=  (stringDN.subString oldIndex (index-oldindex)) + otherValue; oldIndex = index + theLength)
									)
									result +=  (stringDN.subString oldIndex)
								)
						)
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				convertToArray()
				useLA = false
				initializeModifiers()
				if (valuePairArray.count == 3) do
				(
					LA = valuePairArray[3]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				someValue = valuePairArray[1]
				otherValue = valuePairArray[2]
					
				ResultArray = case of
				(
					(not subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for item in ResultArray collect (if (Equals (LA item) someValue) then otherValue else item))
															else
															(for item in ResultArray collect (if (Equals item someValue) then otherValue else item))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for item in ResultArray collect (if ((toLower (LA item)) == (toLower someValue)) then otherValue else item))
															else
															(for item in ResultArray collect (if ((toLower item) == (toLower someValue)) then otherValue else item))
														)
								default: 
														(
															if useLA then
															(for item in ResultArray collect (if ((LA item) == someValue) then otherValue else item))
															else
															(for item in ResultArray collect (if (item == someValue) then otherValue else item))
														)
							)
						)
					(subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for item in ResultArray collect (for subItem in item collect (if (Equals (LA subItem) someValue) then otherValue else subItem)))
															else
															(for item in ResultArray collect (for subItem in item collect (if (Equals subItem someValue) then otherValue else subItem)))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for item in ResultArray collect (for subItem in item collect (if ((toLower (LA subItem)) == (toLower someValue)) then otherValue else subItem)))
															else
															(for item in ResultArray collect (for subItem in item collect (if ((toLower subItem) == (toLower someValue)) then otherValue else subItem)))
														)
								default: 
														(
															if useLA then
															(for item in ResultArray collect (for subItem in item collect (if ((LA subItem) == someValue) then otherValue else subItem)))
															else
															(for item in ResultArray collect (for subItem in item collect (if (subItem == someValue) then otherValue else subItem)))
														)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn ReplaceAt TriArray =		--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex): Replaces the occurrence of 'someValue' at 'atIndex' position with 'otherValue' if the match is true
													--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, "s:"): Same as above but searchs 'someValue' in the subarray elements. 
													--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, "ci:"): forces a case Insensitive comparison for strings
													--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, <LambdaExpression var>):  Determines the equality to 'someValue' of the array element at 'atIndex' position after apllying it the Lambda Expression (i.e., item must evaluate to 'someValue').
													--	Accepted modifiers: s, ci, q
													--	'someValue' and 'otherValue' can be an array. In this case, default array equality is used (allways case sensitive)
													--
													-- 	'String maxLINQ':
													--	<StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex): Replaces the occurrence of 'stringToReplace' at 'atIndex' position with 'ReplacingString' if the match is true
													-- 	<StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex, "ci:"): same as above using case insensitive comparison
													--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (TriArray.count == 4) do (LA = TriArray[4]; soft = softParser LA)
					someValue = TriArray[1]
					otherValue = TriArray[2]
					atIndex = TriArray[3]
					if someValue != "" do
					(
						case of
						(
							(caseSensitive):
								(
									theLength = someValue.count
									checkString = substring ResultArray atIndex theLength
									if checkString == someValue do (ResultArray = subString ResultArray 1 (atIndex-1) + otherValue + substring ResultArray (atIndex+theLength) -1)
								)
							(not caseSensitive):
								(
									theLength = someValue.count
									checkString = toLower (substring ResultArray atIndex theLength)
									if checkString == (toLower someValue) do (ResultArray = subString ResultArray 1 (atIndex-1) + otherValue + substring ResultArray (atIndex+theLength) -1)
								)
						)
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if (TriArray.count == 4) do
				(
					LA = TriArray[4]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				someValue = TriArray[1]
				otherValue = TriArray[2]
				atIndex = TriArray[3]
					
				case of
				(
					(not subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(if (Equals (LA ResultArray[atIndex]) someValue) do (ResultArray[atIndex] = otherValue))
															else
															(if (Equals ResultArray[atIndex] someValue) do (ResultArray[atIndex] = otherValue))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(if (toLower (LA ResultArray[atIndex]) == (toLower someValue)) do (ResultArray[atIndex] = otherValue))
															else
															(if (toLower (ResultArray[atIndex]) == (toLower someValue)) do (ResultArray[atIndex] = otherValue))
														)
								default: 
														(
															if useLA then
															(if (LA ResultArray[atIndex] == someValue) do (ResultArray[atIndex] = otherValue))
															else
															(if (ResultArray[atIndex] == someValue) do (ResultArray[atIndex] = otherValue))
														)
							)
						)
					(subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for item in ResultArray do (if (Equals (LA item[atIndex]) someValue) do item[atIndex] = otherValue))
															else
															(for item in ResultArray do (if (Equals item[atIndex] someValue) do item[atIndex] = otherValue))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for item in ResultArray do (if ((toLower (LA item[atIndex])) == (toLower someValue)) do item[atIndex] = otherValue))
															else
															(for item in ResultArray do (if ((toLower item[atIndex]) == (toLower someValue)) do item[atIndex] = otherValue))
														)
								default: 
														(
															if useLA then
															(for item in ResultArray do (if ((LA item[atIndex]) == someValue) do item[atIndex] = otherValue))
															else
															(for item in ResultArray do (if (item[atIndex] == someValue) do item[atIndex] = otherValue))
														)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Remove someValue =		--	<ArrayQuery>.Remove(someValue): Removes all occurrences of 'someValue' in the array.
													--	<ArrayQuery>.Remove#(someValue, "s:"): Same as above but removes 'someValue' in the subarray elements. 
													--	<ArrayQuery>.Remove#(someValue, "ci:"): forces a case Insensitive comparison for strings
													--	<ArrayQuery>.Remove#(someValue, <LambdaExpression var>):  Determines the equality to 'someValue' of the array elements after apllying them the Lambda Expression (i.e., items that evaluate to 'someValue' are removed).
													--	Accepted modifiers: s, ci, q
													--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
													--
													-- 	'String maxLINQ':
													--	<StringQuery>.Remove(<stringToRemove>):Removes all occurrences of 'stringToRemove'.
													-- 	<StringQuery>.Remove#(<stringToRemove>, "ci:"): same as above using case insensitive comparison
													--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf someValue Array) do (LA = someValue[2]; someValue = someValue[1]; soft = softParser LA)
					
					if someValue != "" do
					(
						otherValue = ""
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						ResultArray = case of
						(
							(caseSensitive):
								(
									stringDN.replace someValue otherValue
								)
							(not caseSensitive):
								(
									theLength = someValue.count
									found = false; result = ""; oldIndex = 0; index = 0
									while not found do
									(
										index = stringDN.IndexOf someValue oldIndex strCompCI
										found = index == -1
										if not found do (result +=  (stringDN.subString oldIndex (index-oldindex)) + otherValue; oldIndex = index + theLength)
									)
									result +=  (stringDN.subString oldIndex)
								)
						)
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if (isKindOf someValue Array) do
				(
					if (someValue.count == 2) and (isKindOf someValue[1] Array) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
					(
						LA = someValue[2]
						someValue = someValue[1]
						soft = softParser LA
						if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
					)
					else
					(
						if (someValue.count == 2) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
							(
								LA = someValue[2]
								soft = softParser LA
								if not soft do (try (useLA = true; soft = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression)) catch (useLA = false; soft = false))
								if soft do (someValue = someValue[1])
							)
					)
				)
				
				case of
				(
					(not subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for i = ResultArray.count to 1 by -1 do (if (Equals (LA ResultArray[i]) someValue) do deleteItem ResultArray i))
															else
															(for i = ResultArray.count to 1 by -1 do (if (Equals ResultArray[i] someValue) do deleteItem ResultArray i))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for i = ResultArray.count to 1 by -1 do (if ((toLower (LA ResultArray[i])) == (toLower someValue)) do deleteItem ResultArray i))
															else
															(for i = ResultArray.count to 1 by -1 do (if ((toLower ResultArray[i]) == (toLower someValue)) do deleteItem ResultArray i))
														)
								default: 
														(
															if useLA then
															(for i = ResultArray.count to 1 by -1 do (if ((LA ResultArray[i]) == someValue) do deleteItem ResultArray i))
															else
															(for i = ResultArray.count to 1 by -1 do (if (ResultArray[i] == someValue) do deleteItem ResultArray i))
														)
							)
						)
					(subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if (Equals (LA item[i]) someValue) do deleteItem item i)))	
															else
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if (Equals item[i] someValue) do deleteItem item i)))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if ((toLower (LA item[i])) == (toLower someValue)) do deleteItem item i)))
															else
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if ((toLower item[i]) == (toLower someValue)) do deleteItem item i)))
														)
								default: 
														(
															if useLA then
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if ((LA item[i]) == someValue) do deleteItem item i)))
															else
															(for item in ResultArray do (for i = item.count to 1 by -1 do (if (item[i] == someValue) do deleteItem item i)))
														)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn RemoveAt valuePairArray =	--	<ArrayQuery>.RemoveAt#(someValue, atIndex): Removes the occurrence of 'someValue' at 'atIndex' position if the match is true
														--	<ArrayQuery>.RemoveAt#(someValue, atIndex, "s:"): Same as above but searchs 'someValue' in the subarray elements. 
														--	<ArrayQuery>.RemoveAt#(someValue, atIndex, "ci:"): forces a case Insensitive comparison for strings
														--	<ArrayQuery>.RemoveAt#(someValue, atIndex, <LambdaExpression var>):  Determines the equality to 'someValue' of the array element at 'atIndex' position after apllying it the Lambda Expression (i.e., item must evaluate to 'someValue').
														--	Accepted modifiers: s, ci, q
														--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
														--
														-- 	'String maxLINQ':
														--	<StringQuery>.RemoveAt#(<stringToRemove>, atIndex): Removes the occurrence of 'stringToRemove' at 'atIndex' position if the match is true
														-- 	<StringQuery>.RemoveAt#(<stringToRemove>, atIndex, "ci:"): same as above using case insensitive comparison
														--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (valuePairArray.count == 3) do (LA = valuePairArray[3]; soft = softParser LA)
					someValue = valuePairArray[1]
					atIndex = valuePairArray[2]
					if someValue != "" do
					(
						case of
						(
							(caseSensitive):
								(
									theLength = someValue.count
									checkString = substring ResultArray atIndex theLength
									if checkString == someValue do (ResultArray = subString ResultArray 1 (atIndex-1) + substring ResultArray (atIndex+theLength) -1)
								)
							(not caseSensitive):
								(
									theLength = someValue.count
									checkString = toLower (substring ResultArray atIndex theLength)
									if checkString == (toLower someValue) do (ResultArray = subString ResultArray 1 (atIndex-1) + substring ResultArray (atIndex+theLength) -1)
								)
						)
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if (valuePairArray.count == 3) do
				(
					LA = valuePairArray[3]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				someValue = valuePairArray[1]
				atIndex = valuePairArray[2]
					
				case of
				(
					(not subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(if (Equals (LA ResultArray[atIndex]) someValue) do (deleteItem ResultArray atIndex))
															else
															(if (Equals ResultArray[atIndex] someValue) do (deleteItem ResultArray atIndex))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(if (toLower (LA ResultArray[atIndex]) == (toLower someValue)) do (deleteItem ResultArray atIndex))
															else
															(if (toLower (ResultArray[atIndex]) == (toLower someValue)) do (deleteItem ResultArray atIndex))
														)
								default: 
														(
															if useLA then
															(if (LA ResultArray[atIndex] == someValue) do (deleteItem ResultArray atIndex))
															else
															(if (ResultArray[atIndex] == someValue) do (deleteItem ResultArray atIndex))
														)
							)
						)
					(subArrayCalc):
						(
							case of
							(
								(isKindOf someValue Array):
														(
															if useLA then
															(for item in ResultArray do (if (Equals (LA item[atIndex]) someValue) do (deleteItem item atIndex)))
															else
															(for item in ResultArray do (if (Equals item[atIndex] someValue) do (deleteItem item atIndex)))
														)
								(not caseSensitive and (isKindOf someValue string)): 
														(
															if useLA then
															(for item in ResultArray do (if ((toLower (LA item[atIndex])) == (toLower someValue)) do (deleteItem item atIndex)))
															else
															(for item in ResultArray do (if ((toLower item[atIndex]) == (toLower someValue)) do (deleteItem item atIndex)))
														)
								default: 
														(
															if useLA then
															(for item in ResultArray do (if ((LA item[atIndex]) == someValue) do (deleteItem item atIndex)))
															else
															(for item in ResultArray do (if (item[atIndex] == someValue) do (deleteItem item atIndex)))
														)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Save &var =	-- <ArrayQuery>.Save(&var).<next query function>: Saves the temporary query result to an existing variable name in the actual scope
									-- Variable name 'var' must exist in the actual scope
									-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
									--
									-- 'String maxLINQ':
									-- This Function is valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) then
				(
					var = Copy ResultArray
				)
				else
				(
					ResultArray = maxLINQ.ToArray ResultArray
					var = deepCopy ResultArray
				)

				return (this)
			),
			fn Debug stringMessage =	-- <ArrayQuery>.Debug#().<next query function>: Prints the temporary query result to Listener
												-- <ArrayQuery>.Debug(string_message).<next query function>: Prints the temporary query result to Listener preceded by the input string message
												-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
												--
												-- 'String maxLINQ':
												-- This Function is valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) then
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
				)
				else
				(
					ResultArray = maxLINQ.ToArray ResultArray
				)
				
				if (Equals stringMessage #()) do stringMessage = ""
				format "%==> %\n" stringMessage ResultArray
				return (this)
			),
			
			
			-- FINAL FUNCTIONS
			fn Show =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).Show(): Returns the result array after previous Query and Filter functions
							--	<ArrayQuery>.Show(): retrieves the last query as array.
							--	Only valid after a function query with 'q:' modifier or as first function to retrieve the last query.
							--
							-- 'String maxLINQ':
							-- This Function is valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) then
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
				)
				else
				(
					ResultArray = maxLINQ.ToArray ResultArray
				)
				return ResultArray
			),
			fn ToArray voidArray =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).ToArray#():  retrieves the last query as array. (same as 'Show()')
											--
											-- 'String maxLINQ':
											--	<StringQuery>[. <sequence of query or filter functions>]).ToArray#(): splits the string into a character array
											--	Accepted modifiers: none
			(
				maxLINQ.ToArray ResultArray
			),
			fn ToString voidArray =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).ToString#(): Converts the result array to a string array
												--
												-- 'String maxLINQ':
												-- This Function is valid for 'String maxLINQ' but does nothing. 
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					return (ResultArray)
				)
				
				ResultArray = maxLINQ.ToArray ResultArray
				Result = for item in ResultArray collect (item as string)
			),
			fn ToFile filename =		--	(<ArrayQuery>[. <sequence of query or filter functions>]).ToFile(<filename string>): Writes the result array to the specified File, one array element by line.
											--	CAUTION! If the specified File exist, it will be overwritten. If not, a new File will be created.
											--
											-- 'String maxLINQ':
											-- This Function is valid for 'String maxLINQ'. Writes the string to the specified File.
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					(dotnetclass "System.IO.File").WriteAllLines filename #(ResultArray)
					return (OK)
				)
				
				ResultArray = maxLINQ.ToArray ResultArray
				if isKindOf ResultArray[1] string then
				(
					(dotnetclass "System.IO.File").WriteAllLines filename ResultArray
				)
				else
				(
					Result = (_From ResultArray).ToString#()
					(dotnetclass "System.IO.File").WriteAllLines filename Result
				)
				OK
			),
			fn Count LA = 		--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count#(): Returns the number of elements in the result array
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count("s:"): Returns an array with the number of elements of each item in the result array
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count(<BooleanLambdaExpression var>): Returns the number of elements in the result array that satisfy the Boolean Lambda Expression.
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression which holds the index of each element
										--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
										--	Accepted modifiers: s, obj
										--
										-- 	'String maxLINQ':
										--	<StringQuery>[. <sequence of query or filter functions>]).Count#(): Returns the length of the string
										--	Accepted modifiers: none
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					return (ResultArray.count)
				)
				
				convertToArray()
				initializeModifiers()
				
				if not (Equals LA #()) then
				(
					soft = softParser LA
					if not soft do (_Where LA)
					
					if subArrayCalc do
					(
						Result = for item in ResultArray collect item.count
						return (Result)
					)
				)
				
				return (ResultArray.count)
			),
			fn Sum LA =		--	(<ArrayQuery>[. <sequence of query or filter functions>]).Sum#(): Returns the standard addition of the array elements
									--	(<ArrayQuery>[. <sequence of query or filter functions>]).Sum("s:"): Returns an array with the standard addition of each subarray element
									--	(<ArrayQuery>[. <sequence of query or filter functions>]).Sum(<LambdaExpression var>):  Returns the addition of the array elements after applying the Lambda Expression to each element. Changes array Values
									--	(<ArrayQuery>[. <sequence of query or filter functions>]).Sum#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Reserved word 'index' in lambda expression which holds the index of each element
									--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
									--	Accepted modifiers: s, obj
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				initializeModifiers()
				
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (Select LA)
				)
				
				ResultSum =
					case of
					(
						(not subArrayCalc): (	
														arrayClass = classof ResultArray[1]
														suma = if arrayClass == string then "" else 0
														for item in ResultArray do (suma += item)
														suma
													)
						(subArrayCalc):		(	
														for item in ResultArray collect (
														arrayClass = classof item[1]
														suma = if arrayClass == string then "" else 0
														for subItem in item do (suma += subItem)
														suma)
													)	
					)
			),
			fn Average LA =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average#(): Returns the standard average value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average("s:"): Returns an array with the standard average of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average(<LambdaExpression var>):  Returns the Average of the array elements after applying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression which holds the index of each element
										--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
										--	Accepted modifiers: s, obj
										--
										-- 'String maxLINQ':
										-- This Function is not valid for 'String maxLINQ'. 
			(
				if (isKindOf ResultArray String) or (isKindOf ResultArray Name) or (isDotNetString ResultArray) do return(Format "--	maxLINQ function not valid for Strings or Names: "; print ((_From ResultArray).toString#()); undefined)
				
				Result = (Sum LA)
				ResultAverage =
					case of
					(
						(not subArrayCalc): (	
														(Result as float) / ResultArray.count
													)
						(subArrayCalc):		(	
														for i = 1 to ResultArray.count collect ((Result[i] as float) / ResultArray[i].count)
													)	
					)
			),
			fn _min LA =		--	(<ArrayQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("i:"): Returns the index of the minimum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("s:"): Returns an array with the minimum value of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("ci:"): makes a case insensitive comparison for strings
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min(<LambdaExpression var>):  Returns the minimum value of the array elements after applying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression which holds the index of each element
										--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
										--	Accepted modifiers: s, i, ci, obj
										--
										-- 	'String maxLINQ':
										--	(<StringQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("i:"): Returns index of the minimum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("ci:"): same as above using case insensitive comparing
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("ci, i:"): same as above using case insensitive comparing and returning the index
										--	Accepted modifiers:  i, ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if not (Equals LA #()) do
					(
						soft = softParser LA
					)
					
					ResultMin = case of
					(
						(caseSensitive and not indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect ResultArray[i]
								amin Result
							)
						(caseSensitive and indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect ResultArray[i]
								theMin = amin Result
								findItem Result theMin
							)
						(not caseSensitive and not indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect toLower ResultArray[i]
								theMin = amin Result
								index = findItem Result theMin
								ResultArray[index]
							)
						(not caseSensitive and indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect toLower ResultArray[i]
								theMin = amin Result
								index = findItem Result theMin
							)
					)
					return (ResultMin)
				)

				
				convertToArray()
				initializeModifiers()
				
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (Select LA)
				)
				
				ResultMin =
					case of
					(
						(not subArrayCalc and not indexCalc):
													(	
														if (not (isKindOf ResultArray[1] string)) or caseSensitive then (amin ResultArray) else
															(lowerResult = for item in ResultArray collect (toLower item); id = finditem lowerResult (amin lowerResult); ResultArray[id])
													)
						(subArrayCalc and not indexCalc):
													(	
														for item in ResultArray collect (
															if (not (isKindOf item[1] string)) or caseSensitive then (amin item) else
																(lowerResult = for subItem in item collect (toLower subItem); id = finditem lowerResult (amin lowerResult); item[id])
															)
													)	
						(not subArrayCalc and indexCalc):
													(	
														if (not (isKindOf ResultArray[1] string)) or caseSensitive then (finditem ResultArray (amin ResultArray)) else
															(lowerResult = for item in ResultArray collect (toLower item); finditem lowerResult (amin lowerResult))
													)
						(subArrayCalc and indexCalc):
													(	
														for item in ResultArray collect (
															if (not (isKindOf item[1] string)) or caseSensitive then (finditem item (amin item)) else
																(lowerResult = for subItem in item collect (toLower subItem); finditem lowerResult (amin lowerResult))
															)
													)	
					)
			),
			fn _max LA =		--	(<ArrayQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("i:"): Returns the index of the maximum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("s:"): Returns an array with the maximum value of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("ci:"): makes a case insensitive comparison for strings
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max(<LambdaExpression var>):  Returns the maximum value of the array elements after apllying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression which holds the index of each element
										--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
										--	Accepted modifiers: s, i, ci, obj
										--
										-- 	'String maxLINQ':
										--	(<StringQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("i:"): Returns index of the maximum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("ci:"): same as above using case insensitive comparing
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("ci, i:"): same as above using case insensitive comparing and returning the index
										--	Accepted modifiers:  i, ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if not (Equals LA #()) do
					(
						soft = softParser LA
					)
					
					ResultMax = case of
					(
						(caseSensitive and not indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect ResultArray[i]
								amax Result
							)
						(caseSensitive and indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect ResultArray[i]
								theMin = amax Result
								findItem Result theMin
							)
						(not caseSensitive and not indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect toLower ResultArray[i]
								theMin = amax Result
								index = findItem Result theMin
								ResultArray[index]
							)
						(not caseSensitive and indexCalc):
							(
								Result = for i = 1 to ResultArray.count collect toLower ResultArray[i]
								theMin = amax Result
								index = findItem Result theMin
							)
					)
					return (ResultMax)
				)
				
				
				convertToArray()
				initializeModifiers()
				
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (Select LA)
				)
				
				ResultMax =
					case of
					(
						(not subArrayCalc and not indexCalc):
													(	
														if (not (isKindOf ResultArray[1] string)) or caseSensitive then (amax ResultArray) else
															(lowerResult = for item in ResultArray collect (toLower item); id = finditem lowerResult (amax lowerResult); ResultArray[id])
													)
						(subArrayCalc and not indexCalc):
													(	
														for item in ResultArray collect (
															if (not (isKindOf item[1] string)) or caseSensitive then (amax item) else
																(lowerResult = for subItem in item collect (toLower subItem); id = finditem lowerResult (amax lowerResult); item[id])
															)
													)	
						(not subArrayCalc and indexCalc):
													(	
														if (not (isKindOf ResultArray[1] string)) or caseSensitive then (finditem ResultArray (amax ResultArray)) else
															(lowerResult = for item in ResultArray collect (toLower item); finditem lowerResult (amax lowerResult))
													)
						(subArrayCalc and indexCalc):
													(	
														for item in ResultArray collect (
															if (not (isKindOf item[1] string)) or caseSensitive then (finditem item (amax item)) else
																(lowerResult = for subItem in item collect (toLower subItem); finditem lowerResult (amax lowerResult))
															)
													)	
					)
			),
			fn All LA =	--	<ArrayQuery>.All(<BooleanLambdaExpression var>): Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
							--	<ArrayQuery>.All#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
							--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
							--	Reserved word 'index' in lambda expression which holds the index of each element
							--	Accepted modifiers: s
							--
							-- 	'String maxLINQ':
							--	<StringQuery>[. <sequence of query or filter functions>]).All(<charString>): Determines whether all characters in the string match the characters in 'charString' string.
							-- 	<StringQuery>[. <sequence of query or filter functions>]).All#(<charString>, "ci:"): same as above using case insensitive comparison
							--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
					Result = false
					if LA != "" do
					(
						Result = case of
						(
							(caseSensitive):
								(
									myString = (_From ResultArray).toArray#()
									checkChar = (_From LA).toArray#()
									notMatched = (_From myString).Except(checkChar)
									(Equals notMatched #()) and (ResultArray != "")
								)
							(not caseSensitive):
								(
									myString = (_From (toLower ResultArray)).toArray#()
									checkChar = (_From (toLower LA)).toArray#()
									notMatched = (_From myString).Except(checkChar)
									(Equals notMatched #()) and (ResultArray != "")
								)
						)
					)
					return (Result)
				)
				
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				resultBool = true
				resultBoolArray = #()

				case of
				(
					(not subArrayCalc):	for id = 1 to ResultArray.count while resultBool do (resultBool = (LA id ResultArray[id]))
					(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = true; for id = 1 to item.count while resultBool do resultBool = (LA id Item[id]); resultBool)
				)
				
				return (if subArrayCalc then resultBoolArray else resultBool)
			),
			fn Any LA =	--	<ArrayQuery>.Any(<BooleanLambdaExpression var>): Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
								--	<ArrayQuery>.Any#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
								--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
								--	Reserved word 'index' in lambda expression which holds the index of each element
								--	Accepted modifiers: s
								--
								-- 	'String maxLINQ':
								--	<StringQuery>[. <sequence of query or filter functions>]).Any(<charString>): Determines whether at least one character in the string match one character in 'charString' string.
								-- 	<StringQuery>[. <sequence of query or filter functions>]).Any#(<charString>, "ci:"): same as above using case insensitive comparison
								--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
					Result = false
					if LA != "" do
					(
						Result = case of
						(
							(caseSensitive):
								(
									myString = (_From ResultArray).toArray#()
									checkChar = (_From LA).toArray#()
									Matched = (_From myString).Intersect(checkChar)
									Matched.count != 0 
								)
							(not caseSensitive):
								(
									myString = (_From (toLower ResultArray)).toArray#()
									checkChar = (_From (toLower LA)).toArray#()
									Matched = (_From myString).Intersect(checkChar)
									Matched.count != 0
								)
						)
					)
					return (Result)
				)
				
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				resultBool = false
				resultBoolArray = #()
				
				case of
				(
					(not subArrayCalc):	for id = 1 to ResultArray.count while not resultBool do resultBool = (LA id ResultArray[id])
					(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = false; for id = 1 to item.count while not resultBool do resultBool = (LA id Item[id]); resultBool)
				)
				
				return (if subArrayCalc then resultBoolArray else resultBool)
			),
			fn First LA =		--	<ArrayQuery>.First(<BooleanLambdaExpression var>): Selects the first array element that satisfy the Boolean Lambda Expression.
									--	<ArrayQuery>.First#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Returns 'undefined' if any element satisfy the Boolean condition
									--	Reserved word 'index' in lambda expression which holds the index of each element
									--	Accepted modifiers: s, i
									--
									-- 'String maxLINQ':
									-- This Function has the same behavior than 'Contains' function whith the 'i:' modifier
									--	<StringQuery>[. <sequence of query or filter functions>]).First(<stringToFind>): Returns the index of the first occurrence of 'stringToFind' (0 if none)
									-- 	<StringQuery>[. <sequence of query or filter functions>]).First#(<stringToFind>, "ci:"): same as above using case insensitive comparison
									--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
					ResultContains = 0
					if LA != "" do
					(
						ResultContains = case of
						(
							(caseSensitive):
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.IndexOf LA) + 1
								)
							(not caseSensitive): 
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.IndexOf LA strCompCI) + 1
								)
						)
					)
					return (ResultContains)
				)

				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				ResultItem =
					case of
					(
						(not subArrayCalc and not indexCalc):	(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id]; counter+=1); if found then ResultArray[counter] else ())
						(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id]; counter+=1); if found then item[counter] else ()))
						(not subArrayCalc and indexCalc):			(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id]; counter+=1); if found then counter else ())
						(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id]; counter+=1); if found then counter else ()))
					)
				
				return (ResultItem)
			),
			fn Last LA =		--	<ArrayQuery>.Last(<BooleanLambdaExpression var>): Selects the last array element that satisfy the Boolean Lambda Expression.
									--	<ArrayQuery>.Last#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Returns 'undefined' if any element satisfy the Boolean condition
									--	Reserved word 'index' in lambda expression which holds the index of each element
									--	Accepted modifiers: s, i
									--
									-- 'String maxLINQ':
									--	<StringQuery>[. <sequence of query or filter functions>]).Last(<stringToFind>): Returns the index of the last occurrence of 'stringToFind' (0 if none)
									-- 	<StringQuery>[. <sequence of query or filter functions>]).Last#(<stringToFind>, "ci:"): same as above using case insensitive comparison
									--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
					ResultContains = 0
					if LA != "" do
					(
						ResultContains = case of
						(
							(caseSensitive):
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.LastIndexOf LA) + 1
								)
							(not caseSensitive): 
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.LastIndexOf LA strCompCI) + 1
								)
						)
					)
					return (ResultContains)
				)
			
				
				convertToArray()
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if external then
				(
					LA2 = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
					fn LA index item = LA2 index item ext
				)
				else
				(
					LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN index " + fnExpression)
				)
				
				ResultItem =
					case of
					(
						(not subArrayCalc and not indexCalc):	(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id]; counter-=1); if found then ResultArray[counter] else ())
						(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id]; counter-=1); if found then item[counter] else ()))
						(not subArrayCalc and indexCalc):			(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id]; counter-=1); if found then counter else ())
						(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id]; counter-=1); if found then counter else ()))
					)
				
				return (ResultItem)
			),
			fn Contains someValue =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).Contains(someValue): Determines whether a sequence contains a specified value. Returns False or True
												--	(<ArrayQuery>[. <sequence of query or filter functions>]).Contains#(someValue, "i:"): Same as above but returns 0 if false or the index of the found element if true. 
												--	(<ArrayQuery>[. <sequence of query or filter functions>]).Contains#(someValue, "s:"): Same as above but searchs the value in the subarray elements. Returns an array of booleans values (or indices with "i:" modifier), one for each subarray.
												--	(<ArrayQuery>[. <sequence of query or filter functions>]).Contains#(someValue, "ci:"): forces a case insensitive comparison for strings
												--	(<ArrayQuery>[. <sequence of query or filter functions>]).Contains#(someValue, <LambdaExpression var>):  Determines the equality of 'someValue' with the array elements after apllying the Lambda Expression to each element of the array. Doesn't change array Values
												--	Accepted modifiers: s, ci, i
												--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
												--	When the "i:" modifier is set, the function returns the index of the first coincidence found.
												--
												-- 	'String maxLINQ':
												--	<StringQuery>[. <sequence of query or filter functions>]).Contains(<stringToFind>): Determines whether a string contains a specified string 'stringToFind'. Returns False or True
												-- 	<StringQuery>[. <sequence of query or filter functions>]).Contains#(<stringToFind>, "ci:"): same as above using case insensitive comparison
												-- 	<StringQuery>[. <sequence of query or filter functions>]).Contains#(<stringToFind>, "i:"): same as above but returns the index of the first occurrence (0 if none)
												--	Accepted modifiers: ci, i
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf someValue Array) do (LA = someValue[2]; someValue = someValue[1]; soft = softParser LA)
					
					ResultContains = false
					if someValue != "" do
					(
						ResultContains = case of
						(
							(caseSensitive and not indexCalc):
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									stringDN.Contains someValue
								)
							(not caseSensitive and not indexCalc):
								(
									stringDN = dotnet.ValueToDotNetObject (toLower ResultArray) dotNetString
									stringDN.Contains (toLower someValue)
								)
							(caseSensitive and indexCalc):
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.IndexOf someValue) + 1
								)
							(not caseSensitive and indexCalc): 
								(
									stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
									(stringDN.IndexOf someValue strCompCI) + 1
								)
						)
					)
					return (ResultContains)
				)
				
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if (isKindOf someValue Array) do
				(
					if (someValue.count == 2) and (isKindOf someValue[1] Array) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
					(
						LA = someValue[2]
						someValue = someValue[1]
						soft = softParser LA
						if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
					)
					else
					(
						if (someValue.count == 2) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
							(
								LA = someValue[2]
								soft = softParser LA
								if not soft do (try (useLA = true; soft = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression)) catch (useLA = false; soft = false))
								if soft do (someValue = someValue[1])
							)
					)
				)
				
				ResultContains =
					if useLA then
					(
						case of
						(
							(not subArrayCalc):
														(	
															if (isKindOf someValue array) then
															(
																Index = 0; found = false
																for i = 1 to ResultArray.count while not found do (found = Equals (LA ResultArray[i]) someValue; Index = i)
																if not found do Index = 0
																if indexCalc then Index else (Index != 0)
															)
															else
															(
																if (not (isKindOf ResultArray[1] string)) or caseSensitive then
																	(searchResult = for item in ResultArray collect (LA item); Index = findItem searchResult someValue;
																	if indexCalc then Index else (Index != 0))
																else
																	(searchResult = for item in ResultArray collect (toLower (LA item)); Index = finditem searchResult (toLower someValue);
																	if indexCalc then Index else (Index != 0))
															)
														)
							(subArrayCalc):
														(	
															if (isKindOf someValue array) then
															(
																for item in ResultArray collect (
																	Index = 0; found = false; for i = 1 to item.count while not found do (found = Equals (LA item[i]) someValue; Index = i)
																	if not found do Index = 0
																	if indexCalc then Index else (Index != 0))
															)
															else
															(
																for item in ResultArray collect (
																	if (not (isKindOf item[1] string)) or caseSensitive then
																		(searchResult = for subItem in item collect (LA subItem); Index = findItem searchResult someValue;
																		if indexCalc then Index else (Index != 0))
																	else
																		(searchResult = for subItem in item collect (toLower (LA subItem)); Index = finditem searchResult (toLower someValue);
																		if indexCalc then Index else (Index != 0))
																	)
															)
														)
						)
					)
					else
					(
						case of
						(
							(not subArrayCalc):
														(	
															if (isKindOf someValue array) then
															(
																Index = 0; found = false
																for i = 1 to ResultArray.count while not found do (found = Equals ResultArray[i] someValue; Index = i)
																if not found do Index = 0
																if indexCalc then Index else (Index != 0)
															)
															else
															(
																if (not (isKindOf ResultArray[1] string)) or caseSensitive then (Index = findItem ResultArray someValue; if indexCalc then Index else (Index != 0)) else
																	(lowerResult = for item in ResultArray collect (toLower item); Index = finditem lowerResult (toLower someValue); if indexCalc then Index else (Index != 0))
															)
														)
							(subArrayCalc):
														(	
															if (isKindOf someValue array) then
															(
																for item in ResultArray collect (
																	Index = 0; found = false; for i = 1 to item.count while not found do (found = Equals item[i] someValue; Index = i)
																	if not found do Index = 0
																	if indexCalc then Index else (Index != 0))
															)
															else
															(
																for item in ResultArray collect (
																	if (not (isKindOf item[1] string)) or caseSensitive then (Index = findItem item someValue; if indexCalc then Index else (Index != 0)) else
																		(lowerResult = for subItem in item collect (toLower subItem); Index = finditem lowerResult (toLower someValue); if indexCalc then Index else (Index != 0))
																	)
															)
														)	
						)
					)
			),
			fn SequenceEqual otherArray =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).SequenceEqual(otherArray): Determines whether two sequences are equal by comparing the elements.
														--	(<ArrayQuery>[. <sequence of query or filter functions>]).SequenceEqual#(otherArray, <LambdaExpression var>): Determines whether two sequences are equal by comparing the elements after applying the Lambda expression to each element of both arrays. Changes array Values
														--	Reserved Lambda expressions ("s:"), for fast calculation in case of subArray modifier without a Lambda Expression Value Comparison
														--	Accepted modifiers: s.
														--  The Default Equality is used for array elements. Returns TRUE if both arrays are equal, FALSE otherwise (or a boolean array in case of 's:' modifier and same arrays size)
														--
														-- 	'String maxLINQ':
														--	<StringQuery>[. <sequence of query or filter functions>]).SequenceEqual(<otherString>): Determines whether two strings are equal. Returns False or True
														-- 	<StringQuery>[. <sequence of query or filter functions>]).SequenceEqual#(<otherString>, "ci:"): same as above using case insensitive comparison
														--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf otherArray Array) do (LA = otherArray[2]; otherArray = otherArray[1]; soft = softParser LA)
					
					areEqual = case of
					(
						(caseSensitive):
							(
								ResultArray == otherArray
							)
						(not caseSensitive):
							(
								(stricmp ResultArray otherArray) == 0
							)
					)
					return (areEqual)
				)
		
				
				ResultArray = maxLINQ.ToArray ResultArray
				useLA = false
				initializeModifiers()
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
				otherArray = maxLinq.ToArray otherArray
				if ResultArray.count != otherArray.count then return false
				
				areEqual =
				if useLA then
				(
					case of
					(
						(not subArrayCalc):	
							(
								ResultArray = for item in ResultArray collect (LA item)
								otherArray = for item in otherArray collect (LA item)
								Equals ResultArray otherArray
							)
						(subArrayCalc):
							(
								ResultArray = for i = 1 to ResultArray.count collect (for item in ResultArray[i] collect (LA item))
								otherArray = for i = 1 to otherArray.count collect (for item in otherArray[i] collect (LA item))
								for i = 1 to ResultArray.count collect (
									(
										Equals ResultArray[i] otherArray[i])
									)
							)
					)
				)
				else
				(
					case of
					(
						(not subArrayCalc):	Equals ResultArray otherArray
						(subArrayCalc):		for i = 1 to ResultArray.count collect (Equals ResultArray[i] otherArray[i])
					)
				)
				
				return areEqual
			),
			fn Compare otherArray =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).Compare#(otherArray, <LambdaExpression var>): Compares two arrays by comparing the result after applying the Lambda expression to each array. The result must be a comparable value.
												--	For subarray calculations, compares each subarray with 'otherArray' (after applying the Lambda expression to each), -i.e. comparison is allways against the same value-
												--	Returns a positive value if original array is greater than otherArray, negative value if lesser or 0 if equal.
												--	Reserved Lambda expressions: none
												--	Accepted modifiers: s, ci
												--
												-- 	'String maxLINQ':
												--	<StringQuery>[. <sequence of query or filter functions>]).Compare(<otherString>): Compares two strings. Returns positive value if greater, negative if lesser or 0 if equal.
												-- 	<StringQuery>[. <sequence of query or filter functions>]).SequenceEqual#(<otherString>, "ci:"): same as above using case insensitive comparison
												--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf otherArray Array) do (LA = otherArray[2]; otherArray = otherArray[1]; soft = softParser LA)
					
					resultCompare = case of
					(
						(caseSensitive):
							(
								case of
								(
									(ResultArray > otherArray): 1
									(ResultArray < otherArray): -1
									default: 0
								)
							)
						(not caseSensitive):
							(
								stricmp ResultArray otherArray
							)
					)
					return (resultCompare)
				)
				
				
				ResultArray = maxLINQ.ToArray ResultArray
				LA = otherArray[2]
				otherArray = otherArray[1]
				parser LA
				LA = execute ("maxLINQ.LambdaFN = fn P3D_ArrayQueryLambdaFN " + fnExpression)
				otherArray = maxLinq.ToArray otherArray
				
				resultCompare =
				case of
				(
					(not subArrayCalc):	
						(
							comparableResultArray = LA ResultArray
							comparableOtherArray = LA otherArray
							if isKindOf comparableResultArray string then
							(
								case of
									(
										(caseSensitive):
											(
												case of
												(
													(comparableResultArray > comparableOtherArray): 1
													(comparableResultArray < comparableOtherArray): -1
													default: 0
												)
											)
										(not caseSensitive):
											(
												stricmp comparableResultArray comparableOtherArray
											)
									)
							)
							else
							(
								comparableResultArray - comparableOtherArray
							)
						)
					(subArrayCalc):
						(
							comparableOtherArray = LA otherArray
							for item in ResultArray collect
								(
									comparableItem = LA item
									if isKindOf comparableItem string then
									(
										case of
											(
												(caseSensitive):
													(
														case of
														(
															(comparableItem > comparableOtherArray): 1
															(comparableItem < comparableOtherArray): -1
															default: 0
														)
													)
												(not caseSensitive):
													(
														stricmp comparableItem comparableOtherArray
													)
											)
									)
									else
									(
										comparableItem - comparableOtherArray
									)
								)
						)
				)
				
				return resultCompare
			),
			fn isEmpty LA =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).isEmpty#(): Determines if the array is empty.
									--	(<ArrayQuery>[. <sequence of query or filter functions>]).isEmpty("s:"): Determines if the subarray elements of the array are empty.
									--	Accepted modifiers: s.
									--	Returns TRUE if the array is empty, FALSE otherwise (or a boolean array in case of 's:' modifier)
									--
									-- 	'String maxLINQ':
									--	<StringQuery>[. <sequence of query or filter functions>]).isEmpty#(): Determines whether the string is void (equal to ""). Returns False or True
									--	Accepted modifiers: none
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) or (isDNstr = isDotNetString ResultArray) do
				(
					if isDNstr != undefined do ResultArray = ResultArray.toString()
					if isName != undefined do ResultArray = ResultArray as String
					
					ResultEmpty = false
					if (Equals LA #()) do
					(
						ResultEmpty = ResultArray == ""
					)
					return (ResultEmpty)
				)
				
				ResultArray = maxLINQ.ToArray ResultArray
				
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
				)
				
				ResultEmpty =
					case of
					(
						(not subArrayCalc):	Equals ResultArray #()
						(subArrayCalc):		for i = 1 to ResultArray.count collect (Equals ResultArray[i] #())
					)
			),
			fn updateObjects voidArray =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).updateObjects#(): updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
														--	Accepted modifiers: none
														--	Returns previous array query result.
														--
														-- 'String maxLINQ':
														-- This Function is not valid for 'String maxLINQ'. 
			(
				try 
				(
					for o in P3D_objMaxLinq do update o
				)
				catch
				(
					Format "--	maxLINQ function Error. Some of the previous queried objects are not GeometryClass objects\n"
				)
				return ResultArray
			),
			
			
			on create do
			(
			)
		)
		::_From = maxLINQstruct
	)
	
	-------------------------------------------------------------
	-- 'maxLINQ' instance of maxLINQManager for Help and Utilities on maxLINQ
	-------------------------------------------------------------
	if ::maxLINQ == undefined do
	(
		struct maxLINQManager
		(
			public
			LambdaFN,
			IndexSort,

			private
			dotNetString = dotNetClass "System.String",
			lookUp = 
			(
				struct lookUp (Key, Value, Count)
			),	
			selSet =
			(
				struct selSet (Object, Type, Index, Count)
			),
			fn compileIndexSort =
			(
				IndexSortClassName = "PathScripts.IndexSort"
				classStr = (
				"
				using System;
				using System.Collections.Generic;
				using System.Linq;	
				
				namespace PathScripts
				{
					public class IndexSort
					{
						public static int[] indices = new int[0];

						public static string[] qSortString(string[] theArray, bool descending = false, bool caseSensitive = false)
						{
							return qSort<string>(theArray, descending, caseSensitive);
						}
						public static int[] qSortInt(int[] theArray, bool descending = false)
						{
							return qSort<int>(theArray, descending);
						}
						public static Single[] qSortFloat(Single[] theArray, bool descending = false)
						{
							return qSort<Single>(theArray, descending);
						}

						public static T[] qSort<T>(T[] theArray, bool descending = false, bool caseSensitive = false)
						{
							int numItems = theArray.Length;
							indices = Enumerable.Range(1, numItems).ToArray();
							if (caseSensitive)
							{
								Array.Sort(theArray, indices, StringComparer.Ordinal);
							}
							else
							{
								Array.Sort(theArray, indices);
							}
							if (descending)
							{
								indices = indices.Reverse().ToArray();
								theArray = theArray.Reverse().ToArray();
							}
							return theArray;
						}

						// Next is not for sorting.
						public static bool isIEnumerable(object obj)
						{
							return obj.GetType().GetMethod(\"GetEnumerator\") != null;
						}
						
					}
					
				}"
				)
			
				compilerParams = dotnetobject "System.CodeDom.Compiler.CompilerParameters"
				dotnet.setlifetimecontrol compilerParams #dotnet
				compilerParams.ReferencedAssemblies.Add("System.dll");
				compilerParams.ReferencedAssemblies.Add("System.Core.dll");
				compilerParams.GenerateInMemory = on
				csharpProvider = dotnetobject "Microsoft.CSharp.CSharpCodeProvider"
				compilerResults = csharpProvider.CompileAssemblyFromSource compilerParams #(classStr)
				dotnet.setlifetimecontrol compilerResults #dotnet
				
				compilerResults.CompiledAssembly 
			),
			
			
			public
			-------------------------
			--	#1[Can be deleted for ditribution from here to next label]
			-------------------------
			---------------------------------------------
			--	HELP Function
			---------------------------------------------
			fn Help =	--	General Help for maxLINQ. List of Functions.
			(
				helpmsg = (
	"
	QUERY FUNCTIONS:(Use a Boolean Lambda Expression. Don't affect to array elements values. Allow further queries)
		_Where: Selects the array elements that satisfy a Boolean Lambda Expression.
		W: same than _Where.
		mappedProperty: Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the Boolean Lambda Expression.
		mappedProp: same than mappedProperty.	
		mappedGeometry: Collects the 'Geometry' of the Geometry_Objects in the array that satisfy the Boolean Lambda Expression.
		mappedGeom: same than mappedGeometry.	
		DotNetIterator: Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the Boolean Lambda Expression.
		TakeWhile: Returns the array elements as long as the Boolean Lambda Expression is true
		SkipWhile: Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
					
	FILTER FUNCTIONS:(Most use a Lambda Expression for projecting array values. Array values change for 'Select'=='Apply' functions. Allow further queries)
		DotNetIteratorGet: Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
		Select: Creates a new array by applying a Lambda Expression to the array elements.
		Apply: same as Select
		GroupBy: Creates a new array by grouping the array elements that evaluate to the same value when applying to them the Lambda Expression
		MakeUnique: Creates a new array with unique elements.
		Distinct: same than MakeUnique.
		Intersect: Creates a new array with the common elements in a second array.
		Except: Creates a new array with the elements of the array that are not in a second Array.
		Union: Creates a new array with all unique elements of the array and a second array.
		Concat: Creates a new array concatenating all elements of the array and a second array.
		flatten: Flattens one level of the array
		flattenAll: Flattens all levels of the array. The result array is a one dimension array.
		Sort: Returns the sorted array.
		orderBy: same than Sort.
		ReOrder: Returns a reordered array based on an arrayIndex order
		Reverse: Reverses the order of the array
		Take: Returns the specified number of contiguous elements from the start of the array
		Skip: Bypasses the specified number of contiguous elements from the start of the array and then returns the remaining elements
		Replace: Replaces all occurrences of a given value in the array with another value.
		ReplaceAt: Replaces the occurrence of a given value at a given index position with another value if the match is true
		Remove: Removes all occurrences of a given value in the array.
		RemoveAt: Removes the occurrence of a given value at a given index position if the match is true
		Save: Saves the temporary query result to an existing variable name in the actual scope
		Debug: Prints the temporary query result to Listener
		
	FINAL FUNCTIONS: (they are the last function in the global query)
		Show: Returns the result array after previous Query and Filter functions.
		ToArray: Same as Show for Array Queries. For String Queries, splits the string into a character array.
		ToString: Converts the result array to a string array
		ToFile: Writes the result array to the specified File, one array element by line.
		Count: Returns the number of elements in the result array
		Sum: Returns the addition of the array elements
		Average: Returns the average value of the array elements
		_min: Returns the minimum value of the array elements
		_max: Returns the maximum value of the array elements
		All: Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
		Any: Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
		First: Selects the first array element that satisfy the Boolean Lambda Expression.
		Last: Selects the last array element that satisfy the Boolean Lambda Expression.
		Contains: Determines whether an array contains a specified value. 
		SequenceEqual: Determines whether two sequences are equal by comparing the elements.
		Compare: Compares two arrays by comparing the result after applying a Lambda expression to each array. The result must be comparable
		isEmpty: Determines if the array is empty.
		updateObjects: updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
		------------------------------------------------------------------------------------------------------
	MANAGER FUNCTIONS:
		maxLINQ.help(): shows this message (summary of functions)
		maxLINQ.helpFN \"<function name>\": for help on each maxLINQ function
		maxLINQ.helpFN \"LA\": for help on LambdExpressions syntax
		maxLINQ.helpMAN(): for help on maxLINQ Manager Functions
		maxLINQ.isInstance <qarray>: checks if <qarray> is a maxLINQ instance
		maxLINQ.dispose <[&]qarray>: voids <qarray> maxLINQ instance
		maxLINQ.createInstance <array>: creates a maxLINQ instance from array
		maxLINQ.Show <qarray>: returns the value of a maxLINQ instance
	MANAGER UTILITIES:
		maxLINQ.NRange <valuePair|valueTriple>: Generates a sequence of numbers within a specified range.
		maxLINQ.SRange <valuePair>: Generates a string with a sequence of especified ASCII characters
		maxLINQ.Repeat <valuePair>: Generates a sequence that contains one repeated value a specified number of times.
		maxLINQ.maxInstances MAXClassType: Returns a maxLINQ instance whose value is an array with all the MaxClassType instances in the scene.
		maxLINQ.readFile filename: Returns a maxLINQ instance whose value is a string array where each element corresponds to a line of the specified File
		maxLINQ.HWND word: Returns all the child windows which name contains the given word. The search is case insensitive
		maxLINQ.toArray enumerable: Returns the enumerable collection as array (valid for ObjectSet, String, Name, PathName, bitArray, ArrayParameter, SelectionSet, DotNet Enumerables...)
		maxLINQ.toLookUp <Key: Value: Count:> : Creates a 'LookUp' struct whith the optionals input 'key', 'value' and 'dimension'.
		maxLINQ.toSelSet <valuePair>: Creates a 'SelSet' struct for querying geometries of Editable_Meshes, Editable_Polys and Trimeshes
	")		
				print helpmsg
				ok
			),
			
			fn helpFN fnName =		-- help on each maxLINQ function and LambdExpressions syntax
			(
				case (toLower fnName) of
				(
	--	LAMBDA EXPRESSIONS SYNTAX
					"la": helpmsg =(
	"
	LAMBDA EXPRESSIONS SYNTAX
			-- Lambda Expressions Syntax: (valid as names #' ' and strings \" \")
			-- (#'[q,s,i,key,d,ci:] var => <var expression>')  : applies <var expression> to each 'var' element of the array
			-- (#'[q,s,i,index,obj:] var => <var [obj,index] expression>')  : applies <var [obj,index] expression> to each 'var' element of the array
			-- #(extVar, #'[q,s,i,index,obj,key,d,ci:] var extVar => <var and extVar expression')  : applies <var and extVar expression> to each 'var' element of the array where 'extVar' is an external variable
			-- (#'[q,s,i:] property var => <var expression>')  : applies <var expression> to each 'var' element wich represents a 'property' of the defined type
			-- (#'[q,s,i,index,d,ci:]) : applies these modifiers to the query without any lambda expression
			-- #() : applies no modifier to the query
		MODIFIERS	
			-- Modifiers [q,s,i,index,obj,key,d,ci:]:
				-- Syntax: separated by comas (',') if more than one and ended by colon (':')
				-- q: indicates that it's not the last function in the query. Thus, the returned value will be a struct, not an array. Valid in all Query and Filter functions.
				-- s: works in subarray elements if the array is a multiarray (valid in most query functions)
				-- i: returns indices of elements instead of the elements value (valid in most query functions)
				-- obj: only valid in the  '_Where' and 'Select' functions (and derivatives 'sum', 'count', 'average', '_min' and '_max').Allows to use the variable 'obj' in the lambda expression which holds the maxObject element of a previous query with 'mappedProperty' or 'mappedGeometry'.
				-- key: only valid in the 'mappedProperty', 'mappedGeometry', 'Select' and 'GroupBy' functions. The result query will be an array of 'lookUp' structs with 3 parameters: key, value and count (see each case in their respective help).
				-- d: only valid in the 'Sort' query function. The sort will be done in descending order
				-- ci: The comparisons will be case Insensitive for strings. NOTE: all functions are CaseSensitive by default(valid in some query functions)
		------------------------------------------------------------------------------------------------------
	")
	--	QUERY FUNCTIONS
					"_where": helpmsg =(		--	101
	"
	 _where LA : Query Function (same as 'W')
					--	<ArrayQuery>._Where(<BooleanLambdaExpression var>): Selects the array elements that satisfy the Boolean Lambda Expression.
					--	<ArrayQuery>._Where#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	Reserved word 'index' in lambda expression, which holds the index of each element
					--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
					--	Accepted modifiers: q, s, i, obj
					--
					-- 'String maxLINQ':
					-- <StringQuery>._Where(<stringToSearch>): Returns an array with the indices of all the matches of the 'stringToSearch' in the original string
					-- <StringQuery>._Where#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
					-- Accepted modifiers: ci
						
					-- Examples for 'Array maxLINQ'
					myVar = 10 -- extVar 
					qarray = _From #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'x => x > 15')								--> #(16, 17, 18, 19, 20)
					qarray = _From #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'i: x => x > 15')							--> #(7, 8, 9, 10, 11)
					qarray = _From #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'i: x => x > 15 and index < 10')		--> #(7, 8, 9)
					qarray = _From #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where#(myVar, #'x myVar => x+myVar > 25')			--> #(16, 17, 18, 19, 20)
					qarray = _From #(10,11,12,13,14,15,16,17,18,19,20); (qarray._Where(#'q: x => x > 15')).sum#()					--> 90
					
					qarray = _From #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where(#'s: subitem => subitem > 18')				--> #(#(19, 20), #(25, 38, 19, 20))
					qarray = _From #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where(#'s,i: subitem => subitem > 18')				--> #(#(10, 11), #(1, 2, 8, 9))
					qarray = _From #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where#(myVar, #'s:x myVar => x+myVar > 25')			--> #(#(16, 17, 18, 19, 20), #(25, 38, 16, 17, 18, 19, 20))
					qarray = _From #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); (qarray._Where(#'q,s: subitem => subitem > 15')).sum(#'s:')	--> #(90, 153)
					
					((_From objects).mappedProperty(#'q:modifiers mod=>isKindOf mod Bend')).W(#'x=>not iskindof obj sphere')	--> --> retreives a multiarray with all Bend type modifiers except for nodes that are of type sphere.
					
					(_From #{1..10})._Where(#'x=>x>5')		--> #(6, 7, 8, 9, 10) (bitArray)
					
					var = dotNetObject \"System.Collections.Generic.List`1[System.Int32]\"; for i = 1 to 10 do var.add i;
					(_From var)._Where(#'x=>x>5')		--> #(6, 7, 8, 9, 10) (DotNet Enumerable)
					
					-- Examples for 'String maxLINQ'
					qarray = _From \"Hello World\"; qarray._Where(\"o\")			--> #(5, 8)
					qarray = _From \"Hello World\"; qarray._Where(\"O\")		--> #()
					qarray = _From \"Hello World\"; qarray._Where#(\"O\",\"ci:\")		--> #(5, 8)
					qarray = _From \"Hello World Hello World\"; qarray._Where(\"llo\")	--> #(3, 15)
					qarray = _From \"Hello World Hello World\"; qarray._Where(\"lla\")	--> #()
	")
					"w": helpmsg =(		--	102
	"
	W LA: Query Function (see '_Where' help)
	")
					"mappedproperty": helpmsg =(		--	103
	"
	mappedProperty LA : Query Function (same as 'mappedProp')
					--	<ArrayQuery>.mappedProperty(<BooleanLambdaExpression property var>): Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the  Boolean Lambda Expression.
					--	Reserved word 'obj' in lambda expression which holds the original maxObject_element
					--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
												--	Key: holds the MaxObject
												--	Value: holds the array with the result properties of the query for the key MaxObject
												--	Count: the number of elements in the array of properties for this MAxObject
					--	Some mapped Properties: verts, faces, edges, selectedVerts, selectedFaces, selectedEdges, children, modifiers, controller.Keys...
					--	Accepted modifiers: q, i, key
					--	Allways returns a multisubarray with each subArray corresponding to the catched properties of each MaxObject in the original array
					--	IMPORTANT NOTE: if you query directly the selected object $ (or its name $Spehere001), without including it in an array (#($) or #($Sphere001))
					--							the mapped property will be taken from the object and all its children and sub-children.
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
					qarray = _From objects; qarray.mappedProperty(#'modifiers mod => isKindOf mod Bend')	--> retreives a multiarray (one for each object) with all Bend type modifiers.
					qarray = _From objects; qarray.mappedProperty(#'modifiers mod => (isKindOf mod Bend) and (isKindOf obj.baseObject Editable_Mesh)')	--> retreives a multiarray (one for each object) with all Bend type modifiers if the node baseObject is an Editable_Mesh.
					qarray = _From $Sphere001; qarray.mappedProperty(#'modifiers mod => isKindOf mod Bend')	--> retreives a multiarray (corresponding to node and its children) with all Bend type modifiers.
					qarray = _From #($Sphere001); qarray.mappedProperty(#'modifiers mod => isKindOf mod Bend')	--> retreives an array with all the Bend type modifiers in the node.
					qarray = _From #($Sphere001.position.controller.z_position.controller); qarray.mappedProperty(#'i: Keys k => k.Time > 20f and k.Value >= 0') --> returns the key indices of keys that satisfy the condition
					qarray = _From geometry;
					(((qarray._Where(#'q:node => isKindOf node Editable_Mesh')		--> step 1: select Editable_Meshes from 'geometry'
							).mappedProperty(#'q:SelectedVerts v => v.pos.z >= 0')		--> step 2: select selectedVerts whose Z coord is greater than 0
							).Apply(#'q,s: v => move v [0,0,10]')									--> step 3: moves these vertex 10 units up in Z_axis
							).UpdateObjects#()																--> step 4: updates geometry cache
					qarray = _From objects;
					(((qarray._Where(#'q:node => isKindOf node.baseObject Box') 	--> step 1: select nodes whose baseObject is a box
							).mappedProp(#'q:modifiers mod => isKindOf mod Bend')		--> step 2: select Bend modifiers from these selected nodes
							)._Where(#'s,q:mod => mod.angle > -25.0')							--> step 3: select the Bend modifiers with angle value greater than -25
							).Apply(#'mod => mod.angle = -25.0')									--> step 4: change the angle value to -25 for these selected modifiers
					qarray = _From objects;
					((qarray.mappedProp(#'q, key:children child => true')				--> step 1: create an array of 'lookUp' structs that hold for each node its children
							).orderBy(#'q,d: nodeStruct => nodeStruct.count')				--> step 2: sort the array by the number of children in descending order 
							).Take(1)																				--> step 3: take the first one (i.e. the node with more children)
	")
					"mappedprop": helpmsg =(		--	104
	"
	mappedProp LA: Query Function (see 'mappedProperty' help)
	")
					"mappedgeometry": helpmsg =(		--	105
	"
	mappedGeometry LA : Query Function (same as 'mappedGeom')
					--	<ArrayQuery>.mappedGeometry(<BooleanLambdaExpression GeometyProperty var>): Collects the 'Geometry' of the Geometry_Objects in the array that satisfy the  Boolean Lambda Expression.
					--	Only valid for Verts, Faces and Edges and their 'Selected' ones, for Editable_Meshes and Editable_Poly
					--	Reserved word 'obj' in lambda expression which holds the original maxObject_element
					--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
												--	Key: holds the geometry object
												--	Value: holds the array with the result of the query for the key object
												--	Count: the number of elements in the array
					--	Accepted modifiers: q, i, key
					--	Allways returns a multisubarray with each subArray corresponding to the catched geometry of each MaxObject in the original array that is Editable_Mesh or Editable_Poly. Other classes are skiped.
					--	IMPORTANT NOTE: if you query directly the selected object $ (or its name $Spehere001), without including it in an array (#($) or #($Sphere001))
					--							the mapped property will be taken from the object and all its children and sub-children.
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
					qarray = _From selection; qarray.mappedGeometry(#'Verts v => v.z > 0')	--> retreives a multiarray (one subarray for each object in selection) with the vertex values (Point3) whose Z coordinate is greater than 0.
					qarray = _From selection; qarray.mappedGeometry(#'i:Verts v => v.z > 0')	--> retreives a multiarray (one subarray for each object in selection) with the indices of the vertex whose Z coordinate is greater than 0.
					qarray = _From #($Editable_Mesh001);											--> take only $Sphere001 node, not its children
					(qarray.mappedGeometry(#'i,q: SelectedVerts v => v.z <= 0')		--> step 1: select indices of selected vertex with Z coord less than 0
							).Select(#'obj,s:v => getNormal obj v')								--> step 2: returns an array whith the normals at these vertex
					qarray = _From geometry;
					((((qarray.mappedGeom(#'i,key,q: SelectedVerts v => v.z > 0')		--> step 1: create an array of 'lookUp' structs that hold for each Editable_Mesh/Editable_Poly in 'Geometry' its selectedVerts indices whose Z coord is greater than 0
							).orderBy(#'q,d: nodeStruct => nodeStruct.count')					--> step 2: sort the array by the number of vertex in descending order
							).Take#(3,#'q:')																		--> step 3: select the three objects with more vertex that satisfy the previous condition
							).Apply(#'q: nodeStruct => meshop.moveVert nodeStruct.key nodeStruct.Value [0,0,10]')	--> step 4: move selected vertex 10 units up in the Z_axis
							).UpdateObjects#()																	--> step 5: updates geometry cache
	")
					"mappedgeom": helpmsg =(		--	106
	"
	mappedGeom LA: Query Function (see 'mappedGeometry' help)
	")
					"dotnetiterator": helpmsg =(		--	107
	"
	DotNetIterator LA : Query Function
					--	<ArrayQuery>.DotNetIterator(<BooleanLambdaExpression CollectionItem>): Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the  Boolean Lambda Expression.
					--	<ArrayQuery>.DotNetIterator#(<externalVar>, <BooleanLambdaExpression CollectionItem extVar>): same as above using an external variable/maxObject
					--	Reserved word 'index' in lambda expression which holds the index of each element
					--	Some Dotnet Iteratable: List<>, \"System.Management.ManagementClass\" instances ...
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
					(
						var = dotNetObject \"System.Collections.Generic.List`1[System.Int32]\"
						for i = 1 to 10 do var.add i
						(_From #(var)).DotNetIterator(#'x=>x>5')		--> #(6, 7, 8, 9, 10)
					)
					(
						var = dotNetObject \"System.Collections.Generic.List`1[System.Int32[]]\"
						for i = 0 to 9 do (subItem = for j = 0 to 9 collect ((10*i)+j); var.add subItem)
						(_From #(var)).DotNetIterator(#'x => x[4]>index*9')		--> #(#(70, 71, 72, 73, 74, 75, 76, 77, 78, 79), #(80, 81, 82, 83, 84, 85, 86, 87, 88, 89), #(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))
					)
					(
						dotnet.loadAssembly \"System.Management.dll\"
						mc = dotNetObject \"System.Management.ManagementClass\" \"Win32_Processor\"
						moc = mc.GetInstances()
						QArray = _From #(moc)
						((QArray.DotNetIteratorGet(#'q:x=>x.Properties')).DotNetIterator(#'s,q:x=>x.name == \"ProcessorId\"')).Apply(#'s:x=>x.value')		--> Get processor ID
					)
					(
						-----------------------------------------------------------
						------ Create a DataGridView to Query its data
						-----------------------------------------------------------
						strType = dotNetClass \"System.String\"
						floatType = dotNetClass \"System.Single\"
						myDGV = dotNetObject \"System.Windows.Forms.DataGridView\"
						myDGV.AllowUserToAddRows = off 
						Column_Titles = #(\"Node\", \"Class\",\"X coord\", \"Y coord\",\"Z coord\")
						Column_Types = #(strType, strType, floatType, floatType, floatType)
						INPUT_DATA =	#(#(\"Sphere001\", \"Editable_Mesh\", 0.0, 0.0 , 0.0), \\ 
												   #(\"Sphere002\", \"Editable_Poly\", 10.0, 10.0 , 10.0), \\
												   #(\"Sphere003\", \"Sphere\", 20.0, 20.0 , 0.0), \\
												   #(\"Sphere004\", \"Editable_Mesh\", 10.0, 20.0 , 0.0), \\
												   #(\"Sphere005\", \"Sphere\", 30.0, 30.0 , 10.0))	
						for i = 1 to 5 do
							(
							myCol = dotNetObject \"System.Windows.Forms.DataGridViewTextBoxColumn\"
							myCol.headerText = myCol.name = Column_Titles[i] 
							myCol.ValueType = Column_Types[i]
							myDGV.columns.add myCol
							)
						for i = 0 to INPUT_DATA.count-1 do myDGV.rows.add INPUT_DATA[i+1] 
						---------------------------------------------------------
						
						-- Get Columns names
						Qdgv = _From #(mydgv.columns)
						columnNames = Qdgv.DotNetIteratorGet(#'x=>x.name')
						format \"columnNames = %\\n\" columnNames
						-- Get Columns whose name contains \"coord\" word
						Qdgv = _From #(mydgv.columns)
						coordColumns = (Qdgv.DotNetIterator(#'q:x=>(_From x.name).Contains(\"coord\")')).Select(#'x=>x.name')
						format \"coordColumns = %\\n\" coordColumns
						-- Get Columns types
						Qdgv = _From #(mydgv.columns)
						columnTypes = Qdgv.DotNetIteratorGet(#'x=>x.ValueType.toString()')
						format \"columnTypes = %\\n\" columnTypes
						-- Get Columns whose type is string
						Qdgv = _From #(mydgv.columns)
						stringColumns = (Qdgv.DotNetIterator(#'q:x=>x.ValueType.toString() == \"System.String\"')).Select(#'x=>x.name')
						format \"stringColumns = %\\n\" stringColumns
						-- Get indices of float columns
						Qdgv = _From #(mydgv.columns)
						floatColumnsIndex = (Qdgv.DotNetIterator(#'q:x=>x.ValueType.toString() == \"System.Single\"')).Select(#'x=>x.Index+1')
						format \"floatColumnsIndex = %\\n\" floatColumnsIndex
						-- Get Row values
						for i = 0 to mydgv.rows.count-1 do
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = Qdgv.DotNetIteratorGet(#'x=>(x.value)')
							format \"row[%] = %\\n\" i rows
						)
						-- Get Row float values
						for i = 0 to mydgv.rows.count-1 do
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							coords = (Qdgv.DotNetIterator(#'q:x=>isKindOf x.value float')).select(#'x => x.value')
							format \"Coords[%] = %\\n\" i coords
						)
						-- Get all values from the DataGridView as multiArray
						data = for i = 0 to mydgv.rows.count-1 collect
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = Qdgv.DotNetIteratorGet(#'x=>x.value')
						)
						format \"DataTable = %\\n\" data
						-- Extract from the above multiArray the float data of 'Sphere' class objects
						Qdgv = _From data
						theSpheresPos = (Qdgv._Where(#'q: x => x[2] == \"Sphere\"')).SkipWhile(#'s:x => isKindOf x string')
						format \"Sphere positions = %\\n\" theSpheresPos
						-- Get 3rd column as array (vertical array):
						col = 3
						columnData3 = for i = 0 to mydgv.rows.count-1 collect
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = (Qdgv.DotNetIterator#(col, #'q: x col=>index == col')	--	Use of index and external value
												).Select(#'x=>x.value')
						)
						columnData3 = (_From columnData3).Flatten#()
						format \"columnData3 = %\\n\" columnData3
					)	
						
	")
					"dotnetiteratorget": helpmsg =(		--	108
	"
	DotNetIteratorGet LA : Filter Function
					--	<ArrayQuery>.DotNetIteratorGet(<LambdaExpression CollectionItem>): Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
					--	<ArrayQuery>.DotNetIteratorGet#(<externalVar>, <LambdaExpression CollectionItem extVar>): same as above using an external variable/maxObject
					--	Reserved word 'index' in lambda expression which holds the index of each element
					--	Some Dotnet Iteratable: List<>, \"System.Management.ManagementClass\" instances ...
					--	Accepted modifiers: q, s
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
					(
						var = dotNetObject \"System.Collections.Generic.List`1[System.Int32]\"
						for i = 1 to 10 do var.add i
						(_From #(var)).DotNetIteratorGet(#'x=>x*5')		--> #(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
					)
					(
						var = dotNetObject \"System.Collections.Generic.List`1[System.Int32[]]\"
						for i = 0 to 9 do (subItem = for j = 0 to 9 collect ((10*i)+j); var.add subItem)
						(_From #(var)).DotNetIteratorGet(#'x => x[index]')		--> #(0, 11, 22, 33, 44, 55, 66, 77, 88, 99)
					)
					(
						dotnet.loadAssembly \"System.Management.dll\"
						mc = dotNetObject \"System.Management.ManagementClass\" \"Win32_Processor\"
						moc = mc.GetInstances()
						QArray = _From #(moc)
						((QArray.DotNetIteratorGet(#'q:x=>x.Properties')).DotNetIterator(#'s,q:x=>x.name == \"ProcessorId\"')).Apply(#'s:x=>x.value')		--> Get processor ID
					)
					(
						-----------------------------------------------------------
						------ Create a DataGridView to Query its data
						-----------------------------------------------------------
						strType = dotNetClass \"System.String\"
						floatType = dotNetClass \"System.Single\"
						myDGV = dotNetObject \"System.Windows.Forms.DataGridView\"
						myDGV.AllowUserToAddRows = off 
						Column_Titles = #(\"Node\", \"Class\",\"X coord\", \"Y coord\",\"Z coord\")
						Column_Types = #(strType, strType, floatType, floatType, floatType)
						INPUT_DATA =	#(#(\"Sphere001\", \"Editable_Mesh\", 0.0, 0.0 , 0.0), \\ 
												   #(\"Sphere002\", \"Editable_Poly\", 10.0, 10.0 , 10.0), \\
												   #(\"Sphere003\", \"Sphere\", 20.0, 20.0 , 0.0), \\
												   #(\"Sphere004\", \"Editable_Mesh\", 10.0, 20.0 , 0.0), \\
												   #(\"Sphere005\", \"Sphere\", 30.0, 30.0 , 10.0))	
						for i = 1 to 5 do
							(
							myCol = dotNetObject \"System.Windows.Forms.DataGridViewTextBoxColumn\"
							myCol.headerText = myCol.name = Column_Titles[i] 
							myCol.ValueType = Column_Types[i]
							myDGV.columns.add myCol
							)
						for i = 0 to INPUT_DATA.count-1 do myDGV.rows.add INPUT_DATA[i+1] 
						---------------------------------------------------------
						
						-- Get Columns names
						Qdgv = _From #(mydgv.columns)
						columnNames = Qdgv.DotNetIteratorGet(#'x=>x.name')
						format \"columnNames = %\\n\" columnNames
						-- Get Columns whose name contains \"coord\" word
						Qdgv = _From #(mydgv.columns)
						coordColumns = (Qdgv.DotNetIterator(#'q:x=>(_From x.name).Contains(\"coord\")')).Select(#'x=>x.name')
						format \"coordColumns = %\\n\" coordColumns
						-- Get Columns types
						Qdgv = _From #(mydgv.columns)
						columnTypes = Qdgv.DotNetIteratorGet(#'x=>x.ValueType.toString()')
						format \"columnTypes = %\\n\" columnTypes
						-- Get Columns whose type is string
						Qdgv = _From #(mydgv.columns)
						stringColumns = (Qdgv.DotNetIterator(#'q:x=>x.ValueType.toString() == \"System.String\"')).Select(#'x=>x.name')
						format \"stringColumns = %\\n\" stringColumns
						-- Get indices of float columns
						Qdgv = _From #(mydgv.columns)
						floatColumnsIndex = (Qdgv.DotNetIterator(#'q:x=>x.ValueType.toString() == \"System.Single\"')).Select(#'x=>x.Index+1')
						format \"floatColumnsIndex = %\\n\" floatColumnsIndex
						-- Get Row values
						for i = 0 to mydgv.rows.count-1 do
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = Qdgv.DotNetIteratorGet(#'x=>(x.value)')
							format \"row[%] = %\\n\" i rows
						)
						-- Get Row float values
						for i = 0 to mydgv.rows.count-1 do
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							coords = (Qdgv.DotNetIterator(#'q:x=>isKindOf x.value float')).select(#'x => x.value')
							format \"Coords[%] = %\\n\" i coords
						)
						-- Get all values from the DataGridView as multiArray
						data = for i = 0 to mydgv.rows.count-1 collect
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = Qdgv.DotNetIteratorGet(#'x=>x.value')
						)
						format \"DataTable = %\\n\" data
						-- Extract from the above multiArray the float data of 'Sphere' class objects
						Qdgv = _From data
						theSpheresPos = (Qdgv._Where(#'q: x => x[2] == \"Sphere\"')).SkipWhile(#'s:x => isKindOf x string')
						format \"Sphere positions = %\\n\" theSpheresPos
						-- Get 3rd column as array (vertical array):
						col = 3
						columnData3 = for i = 0 to mydgv.rows.count-1 collect
						(
							Qdgv = _From #(mydgv.rows.item[i].cells)
							rows = (Qdgv.DotNetIterator#(col, #'q: x col=>index == col')	--	Use of index and external value
												).Select(#'x=>x.value')
						)
						columnData3 = (_From columnData3).Flatten#()
						format \"columnData3 = %\\n\" columnData3
					)	
	")
					"takewhile": helpmsg =(		--	109
	"
	TakeWhile LA : Query Function
						--	<ArrayQuery>.TakeWhile(<BooleanLambdaExpression var>): Returns the array elements as long as the Boolean Lambda Expression is true
						--	<ArrayQuery>.TakeWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Reserved word 'index' in lambda expression which holds the index of each element
						--	Accepted modifiers: q, s, i
						--
						-- 'String maxLINQ':
						-- <StringQuery>.TakeWhile(<stringToSearch>): Returns the string before the first occurrence of 'stringToSearch'
						-- <StringQuery>.TakeWhile#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).takeWhile(#'x => x < 17')		--> #(11, 12, 13, 15, 16)
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).takeWhile(#'i:x => x < 17')		--> #(1, 2, 3, 4, 5)
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).takeWhile(#'x => x < 17 and index < 4')	--> #(11, 12, 13)
							a = #(11,12,13,15,16,17,18,15,19,20); sp = 5; (_From a).takeWhile#(sp, #'x sp => (x+sp) < 22')		--> #(11, 12, 13, 15, 16)
							a = #(11,12,13,15,16,17,18,15,19,20); ((_From a).takeWhile(#'q:x => x < 17')).Apply(#'x => x*10')	--> #(110, 120, 130, 150, 160)
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).takeWhile(#'s:x=>x != 5')		--> #(#(1, 2, 3, 4), #(6, 7, 8, 9, 10), #(3, 4))
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).takeWhile(#'s,i:x=>x != 5')	--> #(#(1, 2, 3, 4), #(1, 2, 3, 4, 5), #(1, 2))
							-- maxLINQ inside maxLINQ
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7))
							((_From a)._Where(#'q:x => (_From x).Any(\"x=>x == 5\")')).takeWhile(#'s:x=>x != 5')		--> #(#(1, 2, 3, 4), #(3, 4))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.takeWhile(\"Wor\")					--> \"Hello \"
							qarray = _From \"Hello World\"; qarray.takeWhile(\"wor\")					--> \"Hello World\"
							qarray = _From \"Hello World\"; qarray.takeWhile#(\"wor\",\"ci:\")	--> \"Hello \"
	")
					"skipwhile": helpmsg =(		--	110
	"
	SkipWhile LA : Query Function
						--	<ArrayQuery>.SkipWhile(<BooleanLambdaExpression var>): Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
						--	<ArrayQuery>.SkipWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Reserved word 'index' in lambda expression which holds the index of each element
						--	Accepted modifiers: q, s, i
						--
						-- 'String maxLINQ':
						-- <StringQuery>.SkipWhile(<stringToSearch>): Returns the string after the first occurrence of 'stringToSearch' ('stringToSearch' included)
						-- <StringQuery>.SkipWhile#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).SkipWhile(#'x => x < 17')		--> #(17, 18, 15, 19, 20)
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).SkipWhile(#'i:x => x < 17')		--> #(6, 7, 8, 9, 10)
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).SkipWhile(#'x => x < 17 and index < 4')	--> #(15, 16, 17, 18, 15, 19, 20)
							a = #(11,12,13,15,16,17,18,15,19,20); sp = 5; (_From a).SkipWhile#(sp, #'x sp => (x+sp) < 22')		--> #(17, 18, 15, 19, 20)
							a = #(11,12,13,15,16,17,18,15,19,20); ((_From a).SkipWhile(#'q:x => x < 17')).Apply(#'x => x*10')	--> #(170, 180, 150, 190, 200)
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).SkipWhile(#'s:x=>x != 5')		--> #(#(5), #(), #(5, 6, 7))
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).SkipWhile(#'s,i:x=>x != 5')	--> #(#(5), #(), #(3, 4, 5))
							-- maxLINQ inside maxLINQ
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7))
							((_From a)._Where(#'q:x => (_From x).Any(\"x=>x == 5\")')).SkipWhile(#'s:x=>x != 5')		--> ##(#(5), #(5, 6, 7))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.SkipWhile(\"Wor\")					--> \"World\"
							qarray = _From \"Hello World\"; qarray.SkipWhile(\"wor\")					--> \"\"
							qarray = _From \"Hello World\"; qarray.SkipWhile#(\"wor\",\"ci:\")	--> \"World\"
	")

	--	FILTER FUNCTIONS
					"select": helpmsg =(		--	201
	"
	Select LA : Filter Function (same as 'Apply')
				--	<ArrayQuery>.Select(<LambdaExpression var>): Creates a new array from input array by applying Lambda Expression to array elements (the result array has the same number of elements). Changes array values. 
				--	<ArrayQuery>.Select#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query.
				--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
											--	Key: holds the original array element value
											--	Value: holds the projected value after applying the Lambda Expression to the array element
											--	Count: the number of elements the projected value
				--	Accepted modifiers: q, s, obj, key
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String maxLINQ'. Use 'Replace' or 'ReplaceAt' functions to modify the string
						
				-- Examples for 'Array maxLINQ'
					a = #(11,12,13,14,15,16,17,18,19,20); (_From a).Select(#'x => x * 10')		--> #(110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
					a = #(11,12,13,14,15,16,17,18,19,20); (_From a).Select(#'x => x * 10 + index')	--> #(111, 122, 133, 144, 155, 166, 177, 188, 199, 210)
					a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Select(#'s: x => x^2')	--> #(#(1, 4, 9, 16, 25), #(36, 49, 64, 81, 100), #(9, 16, 25, 36, 49))
						
					numbers = #( 5, 4, 1, 3, 9, 8, 6, 7, 2, 0);
					strings = #(\"zero\", \"one\", \"two\", \"three\", \"four\", \"five\", \"six\", \"seven\", \"eight\", \"nine\");
					(_From numbers).Select#(strings,#'num strings => strings[num+1]')		--> #(\"five\", \"four\", \"one\", \"three\", \"nine\", \"eight\", \"six\", \"seven\", \"two\", \"zero\")
					numbers = #( 5, 4, 1, 3, 9, 8, 6, 7, 2, 0);
					strings = #(\"zero\", \"one\", \"two\", \"three\", \"four\", \"five\", \"six\", \"seven\", \"eight\", \"nine\");
					(_From numbers).Select#(strings,#'num strings => \"The number \" + strings[num+1] + \" is \" + (if (mod num 2) == 0 then \"even\" else \"odd\")')		--> #(\"The number five is odd\", \"The number four is even\",....) 
					numbers = #( 5, 3, 1, 4, 9, 6, 7, 2, 8, 0);
					(_From numbers).Select(#'num => InPlace = (num == index)')		--> #(false, false, false, true, false, true, true, false, false, false)
						
					sp = Box isSelected:true;(_From $).Select(#'node => getPropNames node')	--> #(#(#height, #length, #lengthsegs, #width, #widthsegs, #mapcoords, #heightsegs, #realWorldMapSize))	
					sp = Box isSelected:true; sp1 = Box height:15 parent:sp; sp2 = Box height:5 parent:sp;
					(_From $).Select(#'node => node.height')		--> #(25.0, 15.0, 5.0)
						
					interfaces = (dotnetClass \"System.Net.NetworkInformation.NetworkInterface\").GetAllNetworkInterfaces()	
					(_From interfaces).Select(#'x=> (x.GetPhysicalAddress()).toString()')		--> get all NetWork IDs
					
					sp = sphere(); bx = box(); tp = teapot();
					(_From objects).Select(#'key:node => getPropNames node')	--> #((lookUp key:$Sphere:Sphere001 @ [0.000000,0.000000,0.000000] value:#(#smooth, #radius, #segs, #mapcoords, #slice, #hemisphere, #sliceFrom, #sliceTo, #chop, #recenter, #realWorldMapSize) count:11), (lookUp key:$Box:Box001 @ [0.000000,0.000000,0.000000] value:#(#height, #length, #lengthsegs, #width, #widthsegs, #mapcoords, #heightsegs, #realWorldMapSize) count:8), (lookUp key:$Teapot:Teapot001 @ [0.000000,0.000000,0.000000] value:#(#smooth, #radius, #segs, #mapcoords, #body, #handle, #spout, #lid, #realWorldMapSize) count:9))
					
					(_from $).Select(#'key:x=> v=(x.SelectedVerts as bitArray)')	--> returns an array of Lookup structs for $ ans its children and subchildren where key:node ; value:bitarray of selected verts ; count:number of selectedVerts of the mesh
					
					var = dotNetObject \"System.Collections.Generic.List`1[System.Int32]\";
					(_From ((var.GetType()).GetMethods())).Select(#'method=>method.name')		--> returns all methods names from a C# List<>
					
					(_From #(node, dummy, helper, sphere)).Select(#'class => classof class')		--> #(MAXWrapper, helper, node, GeometryClass)
					
					----
					fileContent = #(\"10.5, 3.7, 0.0\", \"13.8, 10.8, 1.0\", \"18.9, 5.2, 1.0\", \"18.9, 3.7, 0.0\")	--> initialize file content for this example
					(_From fileContent).toFile(\"Z_text.txt\")		--> write to the \"Z_text.txt\" file
					coords = undefined		--> external local variable to hold temporary results
											
					qarray = maxLINQ.readFile(\"Z_text.txt\")							--> read file to a maxLINQ struct
					((((qarray.Select(#'q:line=>filterString line \",\"')		--> split each line by commas
									).Select(#'s,q:x=>x as float')							--> convert each subarray value to float
									).Select(#'q:coord=>[coord[1],coord[2],coord[3]]')	--> create Point3 from float values
									).Save(&coords)													--> save Point3 array to local variable 'coords' for later use
									).Select(#'pos=>point pos:pos')							--> create a point object in each Point3 value
	")
					"apply": helpmsg =(		--	202
	"
	 Apply : Filter Function (see 'Select' help)
	")
					"groupby": helpmsg =(		--	203
	"
	GroupBy LA : Filter Function
					--	<ArrayQuery>.GroupBy(<LambdaExpression var>): Creates a new array from input array by grouping the array elements that evaluate to the same value when applying to them the Lambda Expression
					--	Uses 'Default Equality' function for comparing arrays (if the result of the Lambda Expression is an array)
					--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
									*	Key: the key value of all elements (the value that all group members evaluate to)
									*	Value: the array with the elements that evaluate to the Key property
									*	Count: the number of elements in the group
					--	Accepted modifiers: q, s, i, key
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						numbers = #( 0, 0, 1, 6, 6, 8, 6, 7, 9, 0, 3, 3); (_From numbers).GroupBy(#'x=>x')	--> #(#(0, 0, 0), #(1), #(6, 6, 6), #(8), #(7), #(9), #(3, 3))
						numbers = #( 0, 0, 1, 6, 6, 8, 6, 7, 9, 0, 3, 3); (_From numbers).GroupBy(#'i:x=>x')	--> #(#(1, 2, 10), #(3), #(4, 5, 7), #(6), #(8), #(9), #(11, 12))
						numbers = #( 0, 0, 1, 6, 6, 8, 6, 7, 9, 0, 3, 3); (_From numbers).GroupBy(#'key:x=> mod x 2')	--> #((lookUp key:0.0 value:#(0, 0, 6, 6, 8, 6, 0) count:7), (lookUp key:1.0 value:#(1, 7, 9, 3, 3) count:5))
						numbers = #( 0, 0, 1, 6, 6, 8, 6, 7, 9, 0, 3, 3);
						(((_From numbers).GroupBy(#'q,key: x => mod x 2')
													)._Where(#'q: group => group.key == 0')
													).Select(#'group => group.value')			--> #(#(0, 0, 6, 6, 8, 6, 0))
						
						numbers = #(#(1,2),#(3,5),#(1,2),#(2,4),#(3,6),#(2,4)); (_From numbers).GroupBy(#'x=>x')	--> #(#(#(1, 2), #(1, 2)), #(#(3, 5)), #(#(2, 4), #(2, 4)), #(#(3, 6)))

						numbers = #(#(1,2,3,2,5),#(2,7,4,5,9),#(1,5,7,3,6),#(2,4)); (_From numbers).GroupBy(#'x => x[1]')	--> #(#(#(1, 2, 3, 2, 5), #(1, 5, 7, 3, 6)), #(#(2, 7, 4, 5, 9), #(2, 4)))
						numbers = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); (_From numbers).GroupBy(#'s:x => x')	--> #(#(#(1), #(2, 2), #(3, 3)), #(#(2), #(7, 7, 7), #(4)), #(#(1, 1, 1), #(3, 3)))
						numbers = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); (_From numbers).GroupBy(#'s,i:x => x')	--> #(#(#(1), #(2, 4), #(3, 5)), #(#(1), #(2, 4, 5), #(3)), #(#(1, 2, 5), #(3, 4)))
	
						strings = #(\"zero\", \"one\", \"two\", \"three\", \"four\", \"five\", \"six\", \"seven\", \"eight\", \"nine\");
						(_From strings).GroupBy(#'str => str.count')		--> #(#(\"zero\", \"four\", \"five\", \"nine\"), #(\"one\", \"two\", \"six\"), #(\"three\", \"seven\", \"eight\"))
						(_From strings).GroupBy(#'i:str => str.count')		--> #(#(1, 5, 6, 10), #(2, 3, 7), #(4, 8, 9))
						(_From strings).GroupBy(#'str => subString str 1 1')		--> #(#(\"zero\"), #(\"one\"), #(\"two\", \"three\"), #(\"four\", \"five\"), #(\"six\", \"seven\"), #(\"eight\"), #(\"nine\"))
						
						sp1 = sphere name:\"Sphere_01\"; sp2 = sphere name:\"Sphere_01\"; sp3 = sphere name:\"Sphere_01\" radius:20; bx1 = box name:\"Box_01\"; bx2 = box name:\"Box_01\";
						(_From objects).GroupBy(#'node => node.name')	--> #(#($Sphere:Sphere_01 @ [0.000000,0.000000,0.000000], $Sphere:Sphere_01 @ [0.000000,0.000000,0.000000], $Sphere:Sphere_01 @ [0.000000,0.000000,0.000000]), #($Box:Box_01 @ [0.000000,0.000000,0.000000], $Box:Box_01 @ [0.000000,0.000000,0.000000]))
						(_From objects).GroupBy(#'key:node => node.name')	--> #((lookUp key:\"Sphere_01\" value:#($Sphere:Sphere_01 @ [0.000000,0.000000,0.000000], $Sphere:Sphere_01 @ [0.000000,0.000000,0.000000], $Sphere:Sphere_01 @ [0.000000,0.000000,0.000000]) count:3), (lookUp key:\"Box_01\" value:#($Box:Box_01 @ [0.000000,0.000000,0.000000], $Box:Box_01 @ [0.000000,0.000000,0.000000]) count:2))
						((_From objects).GroupBy(#'q: node => node.name'))._Where(#'group => group.count > 1')  --> Select only groups of objects with the same name with more than one object
						sps = getClassInstances Sphere; (_From sps).GroupBy(#'key:node => node.radius')		--> #((lookUp key:25.0 value:#(Sphere, Sphere) count:2), (lookUp key:20.0 value:#(Sphere) count:1))
						sps = maxLINQ.maxinstances sphere;  sps.GroupBy(#'key:node => node.radius')	--> same as above
						(_From Objects).groupBy(#'key:node => classof node')		--> group objects in the scene by they class.
	")
					"makeunique": helpmsg =(		--	204
	"
	MakeUnique LA : Filter Function (same as 'Distinct')
					--	<ArrayQuery>.MakeUnique#(): Creates a new array with unique elements. Uses 'Default Equality' function for comparing arrays
					--	<ArrayQuery>.MakeUnique(<LambdaExpression>): compares array elements after applying the Lambda expression to each element of the array
					--	Reserved Lambda expressions (\"s:\"), (\"i:\"), (\"i,s:\") and (\"s,i:\") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						myArray = for i = 1 to 20 collect (random 0 20)		--> #(3, 3, 16, 17, 8, 15, 3, 12, 15, 7, 6, 19, 4, 15, 4, 8, 16, 12, 16, 5)
						(_From myArray).makeUnique#()									--> #(3, 16, 17, 8, 15, 12, 7, 6, 19, 4, 5)
						(_From myArray).makeUnique(\"i:\")							--> #(1, 3, 4, 5, 6, 8, 10, 11, 12, 13, 20)
						(_From myArray).makeUnique(#'x => mod x 10')			--> #(3, 16, 17, 8, 15, 12, 19, 4)
						
						numbers = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); (_From numbers).makeUnique(#'s:')	--> #(#(1, 2, 3), #(2, 7, 4), #(1, 3))
						numbers = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); (_From numbers).makeUnique(#'s,i:')	--> #(#(1, 2, 3), #(1, 2, 3), #(1, 3))
						
						((_From objects).Select(#'q: node => node.mat')).makeUnique#()	--> Get all unique materials from scene
						
	")
					"distinct": helpmsg =(		--	205
	"
	 Distinct : Filter Function (see 'MakeUnique' help)
	")
					"intersect": helpmsg =(		--	206
	"
	Intersect otherArray : Filter Function
					--	<ArrayQuery>.Intersect(otherArray): Creates a new array with the common elements of both arrays. Uses 'Default Equality' function for comparing arrays.
					--	<ArrayQuery>.Intersect#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
					--	Reserved Lambda expressions (\"s:\"), (\"i:\"), (\"i,s:\") and (\"s,i:\") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
					--	This function doesn't 'MakeUnique' result. If one item in the original array is duplicated and it exists in 'otherArray', it will be catched twice.
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						numbersA = #( 0, 2, 4, 5, 6, 8, 9 ) 
						numbersB = #( 1, 3, 5, 7, 8 ) 
						(_From numbersA).Intersect(numbersB)					--> #(5, 8)
						(_From numbersA).Intersect#(numbersB,\"i:\")		--> #(4, 6)
						
						numbersA = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); 
						numbersB = #(#(1,2),#(7),#(1,1,3,3,1));
						(_From numbersA).Intersect(numbersB)					--> #(#(1, 1, 3, 3, 1))
						(_From numbersA).Intersect#(numbersB,\"i:\")		--> #(3)
						(_From numbersA).Intersect#(numbersB,\"s:\")		--> #(#(1, 2, 2), #(7, 7, 7), #(1, 1, 3, 3, 1))
						(_From numbersA).Intersect#(numbersB,\"s,i:\")	--> #(#(1, 2, 4), #(2, 4, 5), #(1, 2, 3, 4, 5))
						
						(_From meditMaterials).Intersect(scenematerials)	--> Get Scene Materials that are in the Material Editor 
						
						(_From #{1..20}).Intersect(#{10..30})	-->	#(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

	")
					"except": helpmsg =(		--	207
	"
	Except otherArray : Filter Function
					--	<ArrayQuery>.Except(otherArray): Creates a new array with the elements of the original array that are not in the 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
					--	<ArrayQuery>.Except#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
					--	Reserved Lambda expressions (\"s:\"), (\"i:\"), (\"i,s:\") and (\"s,i:\") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
					--	This function doesn't 'MakeUnique' result. If one item in the original array is duplicated and it doesn't exist in 'otherArray', it will be catched twice
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						numbersA = #( 0, 2, 4, 5, 6, 8, 9 ) 
						numbersB = #( 1, 3, 5, 7, 8 ) 
						(_From numbersA).Except(numbersB)					--> #(0, 2, 4, 6, 9)
						(_From numbersA).Except#(numbersB,\"i:\")		--> #(1, 2, 3, 5, 7)
						
						numbersA = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); 
						numbersB = #(#(1,2),#(7),#(1,1,3,3,1));
						(_From numbersA).Except(numbersB)					--> #(#(1, 2, 3, 2, 3), #(2, 7, 4, 7, 7))
						(_From numbersA).Except#(numbersB,\"i:\")		--> #(1, 2)
						(_From numbersA).Except#(numbersB,\"s:\")		--> #(#(3, 3), #(2, 4), #())
						(_From numbersA).Except#(numbersB,\"s,i:\")	--> #(#(3, 5), #(1, 3), #())
						
						(_From meditMaterials).Except(scenematerials)	--> get 'Material Editor' materials that are not in the scene
						
						theSpheres = (_From objects)._Where(#'node=>isKindOf node Sphere');
						(_From geometry).Except(theSpheres)	--> get all objects except the spheres
						
						(_From geometry).Except($sphe*)	--> get all objects except those whose name starts by \"sphe\"
						
						(_From objects).except(cameras)	--> get all objects except cameras
						(_From objects).except(#() + cameras + lights)		--> get all objects except cameras and lights
						
						(_From #{1..20}).Except(#(1,2,3,4,5,6,7,8,9))	--> #(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
						(_From #{1..20}).Except(#{1..15})	--> #(16, 17, 18, 19, 20)
						(_From ($.verts as bitarray)).Except($.SelectedVerts as bitarray) --> Returns not selected vertex (not the best performance way to do it!)
					
	")
					"union": helpmsg =(		--	208
	"
	Union otherArray : Filter Function
					--	<ArrayQuery>.Union(otherArray): Creates a new array with all unique elements of original array and 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
					--	<ArrayQuery>.Union#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
					--	Reserved Lambda expressions (\"s:\"), (\"i:\"), (\"i,s:\") and (\"s,i:\") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
					--	This function does a 'MakeUnique' result. Duplicated elements will desappear. When returning indices, the ones from the second array start at: index = (original array).count +1
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						numbersA = #( 0, 2, 4, 5, 6, 8, 8 ) 
						numbersB = #( 1, 3, 5, 7, 8 ) 
						(_From numbersA).Union(numbersB)					--> #(0, 2, 4, 5, 6, 8, 1, 3, 7)
						(_From numbersA).Union#(numbersB,\"i:\")		--> #(1, 2, 3, 4, 5, 6, 8, 9, 11)
						
						numbersA = #(#(1,2,3,2,3),#(2,7,4,7,7),#(1,1,3,3,1)); 
						numbersB = #(#(1,2),#(7),#(1,1,3,3,1));
						(_From numbersA).Union(numbersB)					--> #(#(1, 2, 3, 2, 3), #(2, 7, 4, 7, 7), #(1, 1, 3, 3, 1), #(1, 2), #(7))
						(_From numbersA).Union#(numbersB,\"i:\")		--> #(1, 2, 3, 4, 5)
						(_From numbersA).Union#(numbersB,\"s:\")		--> #(#(3, 3), #(2, 4), #())
						(_From numbersA).Union#(numbersB,\"s,i:\")		--> #(#(1, 2, 3), #(2, 7, 4), #(1, 3))
						
						(_From meditMaterials).Union(scenematerials)	--> get all unique 'Material Editor' materials plus 'Scene Materials'
						
						theSpheres = (_From objects)._Where(#'node=>isKindOf node Sphere');
						(_From theSpheres).Union((_From objects)._Where(#'node=>isKindOf node Box'))	--> get all spheres and boxes
						
						(_From geometry).Union(cameras)	--> get all geometry objects and cameras
						
						(_From #{1..20}).Union(#(1,2,3,4,5,6,7,8,9))	--> #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
						(_From (#{1..7})).Union(#{5..10} as array)		--> #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

						(_From $sphe*).Union($box*) --> PathName. Get all unique objects whose names start by 'sphe' and 'box'
						
	")
					"flatten": helpmsg =(		--	209
	"
	flatten : Filter Function
				--	<ArrayQuery>.Flatten#(): Flattens one level of the array
				--	<ArrayQuery>.Flatten(\"q:\"): same than above but not final result
				--	Accepted modifiers: q
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String maxLINQ'. 
						
				-- Examples for 'Array maxLINQ'
					numbers = #(#(1,2),#(3,5),#(1,2),#(2,4),#(3,6),#(2,4));
					(_From numbers).Flatten#()		--> #(1, 2, 3, 5, 1, 2, 2, 4, 3, 6, 2, 4)
						
					numbers = #(#(#(1,2),#(3,5),#(1,2)),#(#(2,4),#(3,6),#(2,4)));
					(_From numbers).Flatten#()		--> #(#(1, 2), #(3, 5), #(1, 2), #(2, 4), #(3, 6), #(2, 4))
	")
					"flattenall": helpmsg =(		--	210
	"
	flattenAll : Filter Function
					--	<ArrayQuery>.flattenAll#(): Flattens all levels of the array. The result array is a one dimension array.
					--	<ArrayQuery>.flattenAll(\"q:\"): same than above but not final result
					--	Accepted modifiers: q
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						numbers = #(#(1,2),#(3,5),#(1,2),#(2,4),#(3,6),#(2,4));
						(_From numbers).flattenAll#()		--> #(1, 2, 3, 5, 1, 2, 2, 4, 3, 6, 2, 4)
							
						numbers = #(#(#(1,2),#(3,5),#(1,2)),#(#(2,4),#(3,6),#(2,4)));
						(_From numbers).flattenAll#()		--> #(1, 2, 3, 5, 1, 2, 2, 4, 3, 6, 2, 4)
	")
					"sort": helpmsg =(		--	211
	"
	Sort LA : Filter Function (same as 'orderBy')
					--	<ArrayQuery>.Sort#(): Returns the sorted array. All the elements must be comparable
					--	<ArrayQuery>.Sort(\"s:\"): Returns a multiarray with the sorted subarrays(valid in combination other modifiers). All the elements in subArrays must be comparable
					--	<ArrayQuery>.Sort(\"i:\"): Returns an array with the sorted index of the original array (valid in combination other modifiers). All the elements must be comparable
					--	<ArrayQuery>.Sort(\"d:\"): Returns the sorted array in descending order (valid in combination other modifiers). All the elements must be comparable
					--	<ArrayQuery>.Sort(\"ci:\"): The sort will be caseInensitive for strings (valid in combination other modifiers). All the elements must be comparable
					--	<ArrayQuery>.Sort(<LambdaExpression var>): Returns the sorted array with the comparing criteria of the item value after applying the Lambda Expression(*). Doesn't change array values.
														(*) Lambda Expression must evaluate to a comparable value. Valid in combination with 's', 'i', 'd' and 'ci' modifiers.
					--	Accepted modifiers: q, s, i, d, ci
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String maxLINQ'. 
						
					-- Examples for 'Array maxLINQ'
						(_From #(2,3,5,6,1,9,4)).Sort#()						--> #(1, 2, 3, 4, 5, 6, 9)
						(_From #(2,3,5,6,1,9,4)).Sort(\"i:\")				--> #(5, 1, 2, 7, 3, 4, 6)
						(_From #(2,3,5,6,1,9,4)).Sort(\"d:\")				--> #(9, 6, 5, 4, 3, 2, 1)
						
						(_From #(#(20,30,50,60,10,90,40),#(41,71,31,81,61))).Sort(\"s:\")	--> #(#(10, 20, 30, 40, 50, 60, 90), #(31, 41, 61, 71, 81))
						(_From #(#(20,30,50,60,10,90,40),#(41,71,31,81,61))).Sort(\"s,i:\")	--> #(#(5, 1, 2, 7, 3, 4, 6), #(3, 1, 5, 2, 4))
						
						(_From #(\"AAA\",\"ccc\",\"DDD\",\"bbb\")).Sort#()				--> #(\"AAA\", \"DDD\", \"bbb\", \"ccc\")
						(_From #(\"AAA\",\"ccc\",\"DDD\",\"bbb\")).Sort(\"ci:\")			--> #(\"AAA\", \"bbb\", \"ccc\", \"DDD\")
						(_From #(\"AAA\",\"ccc\",\"DDD\",\"bbb\")).Sort(\"ci,d:\")		--> #(\"DDD\", \"ccc\", \"bbb\", \"AAA\")
						
						(_From objects).Sort(#'d:x=>x.name')		--> sort objects by name in descending order (case sensitive)
						(_From objects).Sort(#'d,ci:x=>x.name')		--> sort objects by name in descending order (case insensitive)
						(_From objects).Sort(#'d,ci,i:x=>x.name')		--> sort objects by name in descending order (case insensitive) and returns its indices in the virtual 'objects' array
						
						((_From objects)._Where(#'q:node=>isKindOf node sphere')).Sort(#'sp=>sp.radius')	--> sort sphere objects by radius
						((_From selection).GroupBy(#'q,key:obj=>obj.numverts')).Sort(#'group=>group.key')	--> group selected objects by number of verts and sort them by this value
						
						((((_From #(1,1,2,2,2,3,3,3,4,5,6,6,6,6,7,8,8)).GroupBy(#'q,key:x=>x')			--> group by value to lookUp structs
																									).Sort(#'q,d:x=>x.count')		--> sort groups by number of items in descending order
																									).Take#(3,\"q:\")							--> take the first 3 ones
																									).Select(#'x=>x.key')				--> return the 3 most frequent values
						
	")
					"orderby": helpmsg =(		--	212
	"
	orderBy LA : Filter Function (see 'Sort' help)
	")
					"reorder": helpmsg =(		--	213
	"
	ReOrder indexArray : Filter Function
						--	<ArrayQuery>.ReOrder(indexArray): Returns the array reordered based on the arrayIndex order.
						--	<ArrayQuery>.ReOrder#(indexArray, \"s:\"): Returns a multiarray with the reordered subarrays based on the arrayIndex subarrays order. 
						--	indexArray must be an integer array (or multiarray) of the same length than the array to sort, one-based index.
						--	The n'th index value in the 'indexArray' determines that the 'index value' element in the original array must be in the n'th position in the result array.
						--	Accepted modifiers: q, s
						--
						-- 'String maxLINQ':
						-- This Function is not valid for 'String maxLINQ'. 
						
						-- Examples for 'Array maxLINQ'
							(_From #(geometry,lights,cameras)).reorder#(3,2,1)	--> #($cameras, $lights, $geometry)
							(_From #(#(geometry,lights,cameras),#(meditmaterials,sceneMaterials))).reorder#(#(#(3,2,1),#(2,1)),\"s:\")	--> #(#($cameras, $lights, $geometry), #(#materialLibrary(), #meditMaterials()))
							
							sp1 = sphere prefix:\"mySP\" radius:10; sp2 = sphere prefix:\"mySP\" radius:40; sp3 = sphere prefix:\"mySP\" radius:20; sp4 = sphere prefix:\"mySP\" radius:30; 
							mySPNames = for o in $mySP* collect o.name --> collect object names: #(\"mySP001\", \"mySP002\", \"mySP003\", \"mySP004\")
							radiusIndex = (_From $mySP*).sort(#'i:x=>x.radius')	--> sort the spheres by radius ascending and get the sorted indices: #(1, 3, 4, 2)
							(_From mySPNames).reorder(radiusIndex)	--> reorder the names with the radius order: #(\"mySP001\", \"mySP003\", \"mySP004\", \"mySP002\")
						
	")
					"reverse": helpmsg =(		--	214
	"
	Reverse LA : Filter Function
						--	<ArrayQuery>.Reverse#(): Reverses the order of the array
						--	<ArrayQuery>.Reverse(\"s:\"): Reverses the order of the sub_arrays of the multiarray
						--	Accepted modifiers: q, s
						--
						-- 	'String maxLINQ':
						--	<StringQuery>.Reverse#(): Reverses the order of the original string
						--	<StringQuery>.Reverse(\"q:\"): Same as above but allows further queries
						--	Accepted modifiers: q
						
						-- Examples for 'Array maxLINQ'
							a = #(11,12,13,15,16,17,18,15,19,20); (_From a).reverse#()		--> #(20, 19, 15, 18, 17, 16, 15, 13, 12, 11)
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).reverse(#'s:')		--> #(#(5, 4, 3, 2, 1), #(10, 9, 8, 7, 6), #(7, 6, 5, 4, 3))
							(_From #{5..10}).reverse#()		--> #(10, 9, 8, 7, 6, 5)
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.reverse#()			--> \"dlroW olleH\"
							
	")
					"take": helpmsg =(		--	215
	"
	Take int32 : Filter Function
							-- <ArrayQuery>.Take(int32): Returns the specified number of contiguous elements 'int32' from the start of the array. Int32 is an integer value
							-- <ArrayQuery>.Take(int32Array): Returns the specified number of contiguous elements in 'int32Array' from the start of each subarray in the array. Int32Array is an array of integers. 's:' modifier is set by default.
							-- <ArrayQuery>.Take#(int32Array, \"s:\"): same as above
							-- <ArrayQuery>.Take#(int32, \"s:\"): Returns the specified number of contiguous elements 'int32' from the start of each subarray in the array. Int32 is an integer value
							-- Accepted modifiers: q, s
							--
							-- 'String maxLINQ':
							-- <StringQuery>.Take(int32): Returns the string truncated to its int32 first characters
							-- <StringQuery>.Take#(int32, \"q:\"): same as above but allows a next String Query
							--	 Accepted modifiers: q
						
							-- Examples for 'Array maxLINQ'
								a = #(11,12,13,15,16,17,18,15,19,20); (_From a).Take(5)		--> #(11, 12, 13, 15, 16)
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Take#(3,#'s:')		--> #(#(1, 2, 3), #(6, 7, 8), #(3, 4, 5)
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Take#(#(3,2,1),#'s:')		--> #(#(1, 2, 3), #(6, 7), #(3))
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Take(#(3,2,1))		--> #(#(1, 2, 3), #(6, 7), #(3)) (same as above)
								(_From #{5..10}).Take(3)		--> #(5, 6, 7)
							
							-- Examples for 'String maxLINQ'
								qarray = _From \"Hello World\"; qarray.Take(5)			--> \"Hello\"
								qarray = _From \"Hello World\"; (qarray.Take#(5,\"q:\")).Contains(\"W\")		--> False
								
	")
					"skip": helpmsg =(		--	216
	"
	Skip int32 : Filter Function
							-- <ArrayQuery>.Skip(int32): Bypasses the specified number of contiguous elements 'int32' from the start of the array and then returns the remaining elements. Int32 is an integer value
							-- <ArrayQuery>.Skip(int32Array): same as above for each subarray in the array. Int32Array is an array of integers. 's:' modifier is set by default.
							-- <ArrayQuery>.Skip#(int32Array, \"s:\"): same as above
							-- <ArrayQuery>.Skip#(int32, \"s:\"): Bypasses the specified number of contiguous elements 'int32' from the start of each subarray in the array and then returns the remaining elements. Int32 is an integer value
							--	 Accepted modifiers: q, s
							--
							-- 'String maxLINQ':
							-- <StringQuery>.Take(int32): Returns the string without its int32 first characters
							-- <StringQuery>.Take#(int32, \"q:\"): same as above but allows a next String Query
							--	 Accepted modifiers: q
						
						
							-- Examples for 'Array maxLINQ'
								a = #(11,12,13,15,16,17,18,15,19,20); (_From a).Skip(5)		--> #(17, 18, 15, 19, 20)
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Skip#(3,#'s:')		--> #(#(4, 5), #(9, 10), #(6, 7))
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Skip#(#(3,2,1),#'s:')		--> #(#(4, 5), #(8, 9, 10), #(4, 5, 6, 7))
								a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7)); (_From a).Skip(#(3,2,1))		--> #(#(4, 5), #(8, 9, 10), #(4, 5, 6, 7)) (same as above)
								(_From #{5..10}).Skip(3)		--> #(8, 9, 10)
							
							-- Examples for 'String maxLINQ'
								qarray = _From \"Hello World\"; qarray.Skip(6)			--> \"World\"
								qarray = _From \"Hello World\"; (qarray.Skip#(6,\"q:\")).Contains(\"W\")		--> True
								
	")
					"save": helpmsg =(		--	217
	"
	Save &var : Filter Function
						-- <ArrayQuery>.Save(&var).<next query function>: Saves the temporary query result to an existing variable name in the actual scope
						-- Variable name 'var' must exist in the actual scope
						-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String maxLINQ'. 
						
						-- Examples for 'Array maxLINQ'
							(
								myLocal = undefined;
								boxNames = (((_From objects)._Where(#'q:node=>isKindOf node Box')).Save(&myLocal)).Select(#'node=>node.name')
								format \"boxNames: %\\n\" boxNames
								format \"myLocal: %\\n\" myLocal
							)		
									--> Returns:
									--> boxNames: #(\"Box001\", \"Box002\")
									--> myLocal: #($Box:Box001 @ [0.000000,0.000000,0.000000], $Box:Box002 @ [0.000000,0.000000,0.000000])
						
							----
							fileContent = #(\"10.5, 3.7, 0.0\", \"13.8, 10.8, 1.0\", \"18.9, 5.2, 1.0\", \"18.9, 3.7, 0.0\")	--> initialize file content for this example
							(_From fileContent).toFile(\"Z_text.txt\")		--> write to the \"Z_text.txt\" file
							coords = undefined		--> external local variable to hold temporary results
													
							qarray = maxLINQ.readFile(\"Z_text.txt\")							--> read file to a maxLINQ struct
							((((qarray.Select(#'q:line=>filterString line \",\"')		--> split each line by commas
											).Select(#'s,q:x=>x as float')							--> convert each subarray value to float
											).Select(#'q:coord=>[coord[1],coord[2],coord[3]]')	--> create Point3 from float values
											).Save(&coords)													--> save Point3 array to local variable 'coords' for later use
											).Select(#'pos=>point pos:pos')							--> create a point object in each Point3 value
						
	")
					"debug": helpmsg =(		--	218
	"
	Debug stringMessage : Filter Function
						-- <ArrayQuery>.Debug#().<next query function>: Prints the temporary query result to Listener
						-- <ArrayQuery>.Debug(string_message).<next query function>: Prints the temporary query result to Listener preceded by the input string message
						-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String maxLINQ'. 
						
						-- Examples for 'Array maxLINQ'
							qarray = _From (filterString \"I don't want to use maxLINQ at all\" \" \")
							((((((qarray.groupBy(#'q:x=>x.count')
											).Debug(\"groupBy\")				--> groupBy==> #(#(\"I\"), #(\"don't\"), #(\"want\"), #(\"to\", \"at\"), #(\"use\", \"all\"), #(\"maxLINQ\"))
											)._Where(#'q:x=>x.count>1')
											).Debug#()								-->  ==> #(#(\"to\", \"at\"), #(\"use\", \"all\"))
											).Select(#'q:x=>x[1]')
											).Debug(\"Select\")					--> Select==> #(\"to\", \"use\")
											).Sum#()									--> \"touse\"
						
							----
							fileContent = #(\"10.5, 3.7, 0.0\", \"13.8, 10.8, 1.0\", \"18.9, 5.2, 1.0\", \"18.9, 3.7, 0.0\")	--> initialize file content for this example
							(_From fileContent).toFile(\"Z_text.txt\")		--> write to the \"Z_text.txt\" file
							coords = undefined		--> external local variable to hold temporary results
													
							qarray = maxLINQ.readFile(\"Z_text.txt\")								--> read file to a maxLINQ struct
							(((((((qarray.Select(#'q:line=>filterString line \",\"')		--> split each line by commas
												).debug(\"Filtered string\")								--> Filtered string==> #(#(\"10.5\", \" 3.7\", \" 0.0\"), #(\"13.8\", \" 10.8\", \" 1.0\"), #(\"18.9\", \" 5.2\", \" 1.0\"), #(\"18.9\", \" 3.7\", \" 0.0\"))
												).Select(#'s,q:x=>x as float')							--> convert each subarray value to float
												).debug(\"Float values\")									--> Float values==> #(#(10.5, 3.7, 0.0), #(13.8, 10.8, 1.0), #(18.9, 5.2, 1.0), #(18.9, 3.7, 0.0))
												).Select(#'q:coord=>[coord[1],coord[2],coord[3]]')	--> create Point3 from float values
												).debug(\"Point3\")												--> Point3==> #([10.5,3.7,0], [13.8,10.8,1], [18.9,5.2,1], [18.9,3.7,0])
												).Save(&coords)													--> save Point3 array to local variable 'coords' for later use
												).Select(#'pos=>point pos:pos')							--> create a point object in each Point3 value
	")
					"concat": helpmsg =(		--	219
	"
	Concat otherArray : Filter Function
						--	<ArrayQuery>.Concat(otherArray): Creates a new array concatenating all elements of original array and 'otherArray'. 
						--	<ArrayQuery>.Concat#(otherArray, <LambdaExpression var>): concatenates both arrays after applying the Lambda expression to each element of both arrays. 
						--	Reserved Lambda expressions (\"s:\") - with optional 'q' modifier - for fast calculation in case of subArray modifier without a Lambda Expression Value
						--	This function doesn't \"MakeUnique\" result.
						--	Accepted modifiers: q, s
						--
						-- 'String maxLINQ':
						-- <StringQuery>.Concat(<stringToAppend>): Appends 'stringToAppend' to the original string
						-- Accepted modifiers: q
						
						-- Examples for 'Array maxLINQ'
							a = #(11,12,13,14); (_From a).Concat(#(13,14,15,19,20))		--> #(11, 12, 13, 14, 13, 14, 15, 19, 20)
							a = #(11,12,13,14); (_From a).Concat#(#(13,14,15,19,20),#'x=>x*2')		--> #(22, 24, 26, 28, 26, 28, 30, 38, 40)
					
							a = #(#(1,2,3,4,5), #(6,7,8,9,10), #(3,4,5,6,7));
							(_From a).Concat#(#(#(1,2,3),#(3,4),#(10)),#'s:')		--> #(#(1, 2, 3, 4, 5, 1, 2, 3), #(6, 7, 8, 9, 10, 3, 4), #(3, 4, 5, 6, 7, 10))
							(_From a).Concat#(#(#(1,2,3),#(3,4),#(10)),#'s:x=>x*2')	--> #(#(2, 4, 6, 8, 10, 2, 4, 6), #(12, 14, 16, 18, 20, 6, 8), #(6, 8, 10, 12, 14, 20))
					
							(_From #{5..10}).Concat(#{20..25})		--> #(5, 6, 7, 8, 9, 10, 20, 21, 22, 23, 24, 25)
					
							(_From lights).concat(cameras) 		--> returns an array with all ligths and cameras from scene
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.concat(\" and City\")			--> \"Hello World and City\"
								
	")
					"replace": helpmsg =(		--	220
	"
	Replace valuePairArray : Filter Function
						--	<ArrayQuery>.Replace#(someValue, otherValue): Replaces all occurrences of 'someValue' in the array with 'otherValue'.
						--	<ArrayQuery>.Replace#(someValue, otherValue, \"s:\"): Same as above but searchs 'someValue' in the subarray elements. 
						--	<ArrayQuery>.Replace#(someValue, otherValue, \"ci:\"): forces a case insensitive comparison for strings
						--	<ArrayQuery>.Replace#(someValue, otherValue, <LambdaExpression var>):  Determines the equality to 'someValue' of the array elements after applying them the Lambda Expression (i.e., items that evaluate to 'someValue').
						--	Accepted modifiers: s, ci, q
						--	'someValue' and 'otherValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--
						-- 'String maxLINQ':
						-- <StringQuery>.Replace#(<stringToReplace>, <ReplacingString>):Replaces all occurrences of 'stringToReplace' with 'ReplacingString'
						-- <StringQuery>.Replace#(<stringToReplace>, <ReplacingString>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(1,2,3,5,6,7,8,5,9,10); qarray.replace#(5,555)				--> #(1, 2, 3, 555, 6, 7, 8, 555, 9, 10)
							qarray = _From #(1,2,3,5,6,7,8,5,9,10); qarray.replace#(0,555,#'x=>mod x 2')		--> #(1, 555, 3, 5, 555, 7, 555, 5, 9, 555)
							qarray = _From #(\"AAA\",\"bbb\",\"aaa\",\"BBB\"); qarray.replace#(\"aaa\",\"CCC\")	--> #(\"AAA\", \"bbb\", \"CCC\", \"BBB\")
							qarray = _From #(\"AAA\",\"bbb\",\"aaa\",\"BBB\"); qarray.replace#(\"aaa\",\"CCC\",\"ci:\")	--> #(\"CCC\", \"bbb\", \"CCC\", \"BBB\")
							qarray = _From #(#(1,2,3,4,5),#(5,4,3,2,1)); qarray.replace#(5,555,#'s:')		--> #(#(1, 2, 3, 4, 555), #(555, 4, 3, 2, 1))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.replace#(\"world\",\"City\")			--> \"Hello World\"
							qarray = _From \"Hello World\"; qarray.replace#(\"world\",\"City\",\"ci:\")		--> \"Hello City\"
							qarray = _From \"Hello World\"; qarray.replace#(\"o\",\"OOO\",\"ci:\")			--> \"HellOOO WOOOrld\"
							qarray = _From #(\"Hello\",\"World\"); qarray.select(#'x=>(_From x).replace#(\"o\",\"_A_\")')	--> #(\"Hell_A_\", \"W_A_rld\")
						
	")
					"replaceat": helpmsg =(		--	221
	"
	ReplaceAt TriArray : Filter Function
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex): Replaces the occurrence of 'someValue' at 'atIndex' position with 'otherValue' if the match is true
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, \"s:\"): Same as above but searchs 'someValue' in the subarray elements. 
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, \"ci:\"): forces a case Insensitive comparison for strings
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, <LambdaExpression var>):  Determines the equality to 'someValue' of the array element at 'atIndex' position after apllying it the Lambda Expression (i.e., item must evaluated to 'someValue').
						--	Accepted modifiers: s, ci, q
						--	'someValue' and 'otherValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--
						-- 'String maxLINQ':
						-- <StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex): Replaces the occurrence of 'stringToReplace' at 'atIndex' position with 'ReplacingString' if the match is true
						-- <StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(1,2,3,4,5,6,7,8,9); qarray.ReplaceAt#(5,555,6)				--> #(1, 2, 3, 4, 5, 6, 7, 8, 9)
							qarray = _From #(1,2,3,4,5,6,7,8,9); qarray.ReplaceAt#(5,555,5)				--> #(1, 2, 3, 4, 555, 6, 7, 8, 9)
							qarray = _From #(1,2,3,4,5,6,7,8,9); qarray.ReplaceAt#(0,555,4,#'x=>mod x 2')	--> #(1, 2, 3, 555, 5, 6, 7, 8, 9)
							qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.ReplaceAt#(\"bbb\",\"ddd\",2)	--> #(\"AAA\", \"BBB\", \"CCC\")
							qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.ReplaceAt#(\"bbb\",\"ddd\",2,\"ci:\")	--> #(\"AAA\", \"ddd\", \"CCC\")
							
							qarray = _From #(#(1,2,3,4,5,6,7,8,9),#(1,2,3,4,5,6,7,8,9));
							qarray.ReplaceAt#(5,555,5,\"s:\")			--> #(#(1, 2, 3, 4, 555, 6, 7, 8, 9), #(1, 2, 3, 4, 555, 6, 7, 8, 9))
							
							qarray = _From #(#(1,2,3,4,5,6,7,8,9),#(1,2,3,4,5,6,7,8,9));
							qarray.ReplaceAt#(0,555,4,#'s:x=>mod x 2')		--> #(#(1, 2, 3, 555, 5, 6, 7, 8, 9), #(1, 2, 3, 555, 5, 6, 7, 8, 9))
							
							qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); 
							qarray.ReplaceAt#(\"bbb\",\"ddd\",2,\"s,ci:\")	--> #(#(\"AAA\", \"ddd\", \"CCC\"),#(\"AAA\", \"ddd\", \"CCC\"))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"o\",\"OO\",6)			--> \"Hello World\"
							qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"o\",\"OO\",5)			--> \"HellOO World\"
							qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"O\",\"OO\",5)		--> \"Hello World\"
							qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"O\",\"OO\",5,\"ci:\")		--> \"HellOO World\"
					
	")
					"remove": helpmsg =(		--	222
	"
	Remove someValue : Filter Function
						--	<ArrayQuery>.Remove(someValue): Removes all occurrences of 'someValue' in the array.
						--	<ArrayQuery>.Remove#(someValue, \"s:\"): Same as above but removes 'someValue' in the subarray elements. 
						--	<ArrayQuery>.Remove#(someValue, \"ci:\"): forces a case Insensitive comparison for strings
						--	<ArrayQuery>.Remove#(someValue, <LambdaExpression var>):  Determines the equality to 'someValue' of the array elements after apllying them the Lambda Expression (i.e., items that evaluate to 'someValue' are removed).
						--	Accepted modifiers: s, ci, q
						--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--
						-- 'String maxLINQ':
						-- <StringQuery>.Remove(<stringToRemove>):Removes all occurrences of 'stringToRemove'.
						-- <StringQuery>.Remove#(<stringToRemove>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(1,2,3,1,2,3,1,2); qarray.remove(3)				--> #(1, 2, 1, 2, 1, 2)
							qarray = _From #(1,2,3,1,2,3,1,2); qarray.remove#(1,#'x=>mod x 2')		--> #(2, 2, 2)
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,2)		--> #(#(3, 1), #(2, 3))
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,#'s:')	--> #(#(2), #(3), #(2, 3), #(2))
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,#'s:x=>mod x 2')	--> #(#(2), #(), #(2), #(2))
							qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.remove(\"bbb\")	--> #(\"AAA\", \"BBB\", \"CCC\")
							qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.remove#(\"bbb\",\"ci:\")	--> #(\"AAA\", \"CCC\")
							qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); qarray.remove#(true,#'ci,s:x=> x<\"C\"')	--> #(#(\"CCC\"), #(\"CCC\"))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.remove#(\"L\",\"ci:\")			--> \"Heo Word\"
							qarray = _From \"Hello World Hello World\"; qarray.remove#(\"hello \",\"ci:\")		--> \"World World\"
						
	")
					"removeat": helpmsg =(		--	223
	"
	RemoveAt valuePairArray : Filter Function
						--	<ArrayQuery>.RemoveAt#(someValue, atIndex): Removes the occurrence of 'someValue' at 'atIndex' position if the match is true
						--	<ArrayQuery>.RemoveAt#(someValue, atIndex, \"s:\"): Same as above but searchs 'someValue' in the subarray elements. 
						--	<ArrayQuery>.RemoveAt#(someValue, atIndex, \"ci:\"): forces a case Insensitive comparison for strings
						--	<ArrayQuery>.RemoveAt#(someValue, atIndex, <LambdaExpression var>):  Determines the equality to 'someValue' of the array element at 'atIndex' position after apllying it the Lambda Expression (i.e., item must evaluate to 'someValue').
						--	Accepted modifiers: s, ci, q
						--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--
						-- 'String maxLINQ':
						-- <StringQuery>.RemoveAt#(<stringToRemove>, atIndex): Removes the occurrence of 'stringToRemove' at 'atIndex' position if the match is true
						-- <StringQuery>.RemoveAt#(<stringToRemove>, atIndex, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(1,2,3,1,2,3,1,2); qarray.removeAt#(2,4)				--> #(1, 2, 3, 1, 2, 3, 1, 2)
							qarray = _From #(1,2,3,1,2,3,1,2); qarray.removeAt#(1,4)				--> #(1, 2, 3, 2, 3, 1, 2)
							qarray = _From #(1,2,3,1,2,3,1,2); qarray.removeAt#(1,4,#'x=>mod x 2')	--> #(1, 2, 3, 2, 3, 1, 2)
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(#(1,2),4)		--> #(#(1, 2), #(3, 1), #(2, 3))
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(#(2,4),4,#'x=>#(x[1]*2,x[2]*2)')	--> #(#(1, 2), #(3, 1), #(2, 3))
							qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(1,1,\"s:\")	--> ##(#(2), #(3, 1), #(2, 3), #(2))
							qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); qarray.removeAt#(\"bbb\",2,\"s,ci:\")	--> #(#(\"AAA\", \"CCC\"), #(\"AAA\", \"CCC\"))
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.removeAt#(\"o\",6)			--> \"Hello World\"
							qarray = _From \"Hello World\"; qarray.removeAt#(\"o\",5)			--> \"Hell World\"
							qarray = _From \"Hello World\"; qarray.removeAt#(\"O\",5,\"ci:\")	--> \"Hell World\"
					
	")

	--	FINAL FUNCTIONS
					"show": helpmsg =(		--	301
	"
	Show : Final Function 
			-- (<ArrayQuery>[.<sequence of query or filter functions>]).Show(): Returns the result array after previous Query and Filter functions
			-- <ArrayQuery>.Show(): retrieves the last query as array.
			-- Only valid after a function query with 'q:' modifier or as first function to retrieve the last query.
			--
			-- 'String maxLINQ':
			-- This Function is valid for 'String maxLINQ'. 
						
			-- Examples for 'Array maxLINQ'
				a = #(11,12,13,15,16,17,18,15,19,20); ((_From a).takeWhile(#'q:x => x < 17')).Show()	--> #(11, 12, 13, 15, 16)	
				a = #(11,12,13,15,16,17,18,15,19,20); Result = (_From a); Result.takeWhile(#'x => x < 17'); Result.Show()	--> #(11, 12, 13, 15, 16)
						
				(_From objects).show() 	--> Returns array of objects in the scene
	")
					"toarray": helpmsg =(		--	302
	"
	 ToArray : Final Function 
					-- (<ArrayQuery>[. <sequence of query or filter functions>]).ToArray#():  retrieves the last query as array. (same as 'Show()')
					--
					-- 'String maxLINQ':
					-- <StringQuery>[. <sequence of query or filter functions>]).ToArray#(): splits the string into a character array
					-- Accepted modifiers: none
						
					-- Examples for 'Array maxLINQ'
	")
					"count": helpmsg =(		--	303
	"
	Count LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count#(): Returns the number of elements in the result array
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count(\"s:\"): Returns an array with the number of elements of each item in the result array
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count(<BooleanLambdaExpression var>): Returns the number of elements in the result array that satisfy the Boolean Lambda Expression.
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
				--	Accepted modifiers: s, obj
				--
				-- 'String maxLINQ':
				-- <StringQuery>[. <sequence of query or filter functions>]).Count#(): Returns the length of the string
				-- Accepted modifiers: none
						
				-- Examples for 'Array maxLINQ'
	")
					"sum": helpmsg =(		--	304
	"
	Sum LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum#(): Returns the standard addition of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum(\"s:\"): Returns an array with the standard addition of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum(<LambdaExpression var>):  Returns the addition of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
				--	Accepted modifiers: s, obj
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String maxLINQ'. 
						
				-- Examples for 'Array maxLINQ'
	")
					"average": helpmsg =(		--	305
	"
	Average LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average#(): Returns the standard average value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average(\"s:\"): Returns an array with the standard average of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average(<LambdaExpression var>):  Returns the Average of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
				--	Accepted modifiers: s, obj
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String maxLINQ'. 
						
				-- Examples for 'Array maxLINQ'
					(_From $).average(#'x => x.height')		--> average of the 'height' of the node and its children and subchildren (valid if all of them have this property)				
	")
					"_min": helpmsg =(		--	306
	"
	_min LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min#(): Returns the minimum value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min(\"i:\"): Returns the index of the minimum value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min(\"s:\"): Returns an array with the minimum value of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min(\"ci:\"): makes a case insensitive comparison for strings
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min(<LambdaExpression var>):  Returns the minimum value of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._min#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
				--	Accepted modifiers: s, i, ci, obj
				--
				-- 'String maxLINQ':
				-- (<StringQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"i:\"): Returns index of the minimum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"ci:\"): same as above using case insensitive comparing
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"ci, i:\"): same as above using case insensitive comparing and returning the index
				-- Accepted modifiers:  i, ci
						
				-- Examples for 'Array maxLINQ'
				
				
				-- Examples for 'String maxLINQ'
					qarray = _From \"helloWorld\"; qarray._min#()			--> \"W\"
					qarray = _From \"helloWorld\"; qarray._min(\"i:\")			--> 6
					qarray = _From \"helloWorld\"; qarray._min(\"ci:\")			--> \"d\"
					qarray = _From \"helloWorld\"; qarray._min(\"i, ci:\")			--> 10
					
	")
					"_max": helpmsg =(		--	307
	"
	_max LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max#(): Returns the maximum value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max(\"i:\"): Returns the index of the maximum value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max(\"s:\"): Returns an array with the maximum value of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max(\"ci:\"): makes a case insensitive comparison for strings
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max(<LambdaExpression var>):  Returns the maximum value of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>])._max#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
				--	Accepted modifiers: s, i, ci, obj
				--
				-- 'String maxLINQ':
				-- (<StringQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"i:\"): Returns index of the maximum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"ci:\"): same as above using case insensitive comparing
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"ci, i:\"): same as above using case insensitive comparing and returning the index
				-- Accepted modifiers:  i, ci
						
				-- Examples for 'Array maxLINQ'
				
				
				-- Examples for 'String maxLINQ'
					qarray = _From \"helloWorld\"; qarray._max#()			--> \"r\"
					qarray = _From \"helloWorld\"; qarray._max(\"i:\")			--> 8
					qarray = _From \"helloWorld\"; qarray._max(\"ci:\")			--> \"W\"
					qarray = _From \"helloWorld\"; qarray._max(\"i, ci:\")			--> 6
				
	")
					"tostring": helpmsg =(		--	308
	"
	ToString voidArray: Final Function
						--	(<ArrayQuery>[. <sequence of query or filter functions>]).ToString#(): Converts the result array to a string array
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String maxLINQ' but do nothing. 

						-- Examples for 'Array maxLINQ'
	")
					"tofile": helpmsg =(		--	309
	"
	ToFile filename: Final Function
						--	(<ArrayQuery>[. <sequence of query or filter functions>]).ToFile(<filename string>): Writes the result array to the specified File, one array element by line.
						--	CAUTION! If the specified File exist, it will be overwritten. If not, a new File will be created.
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String maxLINQ'. Writes the string to the specified File.

						-- Examples for 'Array maxLINQ'
							fileContent = #(\"10.5, 3.7, 0.0\", \"13.8, 10.8, 1.0\", \"18.9, 5.2, 1.0\", \"18.9, 3.7, 0.0\")	--> initialize file content for this example
							(_From fileContent).toFile(\"Z_text.txt\")		--> write to the \"Z_text.txt\" file
	")
					"all": helpmsg =(		--	310
	"
	All LA : Final Function
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).All(<BooleanLambdaExpression var>): Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).All#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
					--	Reserved word 'index' in lambda expression which holds the index of each element
					--	Accepted modifiers: s
					--
					-- 'String maxLINQ':
					-- <StringQuery>[. <sequence of query or filter functions>]).All(<charString>): Determines whether all characters in the string match the characters in 'charString' string.
					-- <StringQuery>[. <sequence of query or filter functions>]).All#(<charString>, \"ci:\"): same as above using case insensitive comparison
					-- Accepted modifiers: ci
						
					-- Examples for 'Array maxLINQ'
						(_From $).All(#'x => iskindof x geometryClass')  --> Checks wheter the node and its children and subchildren are GeometryClass
					
					-- Examples for 'String maxLINQ'
						(_From \"#\taaa bbb\").All(\"ab \t#\")		--> True
						(_From \"#\taaa Bbb\").All(\"ab \t#\")		--> False
						(_From \"#\taaa bbb\").All#(\"ab \t#\",\"ci:\")		--> True
	")
					"any": helpmsg =(		--	311
	"
	Any LA : Final Function
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).Any(<BooleanLambdaExpression var>): Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).Any#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
					--	Reserved word 'index' in lambda expression modifier which holds the index of each element
					--	Accepted modifiers: s
					--
					--
					-- 	'String maxLINQ':
					--	<StringQuery>[. <sequence of query or filter functions>]).Any(<charString>): Determines whether at least one character in the string match one character in 'charString' string.
					-- 	<StringQuery>[. <sequence of query or filter functions>]).Any#(<charString>, \"ci:\"): same as above using case insensitive comparison
					--	Accepted modifiers: ci
						
					-- Examples for 'Array maxLINQ'
						(_From $).Any(#'x => iskindof x geometryClass')  --> Checks wheter the node or its children and subchildren are a GeometryClass object
					
					-- Examples for 'String maxLINQ'
						(_From \"Hello World\").Any(\"aeiou\")		--> True
						(_From \"Hello World\").Any(\"AEIOU\")		--> True
						(_From \"Hello World\").Any#(\"AEIOU\",\"ci:\")		--> True
	")
					"first": helpmsg =(		--	312
	"
	First LA : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).First(<BooleanLambdaExpression var>): Selects the first array element that satisfy the Boolean Lambda Expression.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).First#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Returns 'undefined' if any element satisfy the Boolean condition
						--	Reserved word 'index' in lambda expression which holds the index of each element
						--	Accepted modifiers: s, i
						--
						-- 'String maxLINQ':
						-- This Function has the same behavior than 'Contains' function whith the 'i:' modifier
						-- (<StringQuery>[.<sequence of query or filter functions>]).First(<stringToFind>): Returns the index of the first occurrence of 'stringToFind' (0 if none)
						-- (<StringQuery>[.<sequence of query or filter functions>]).First#(<stringToFind>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
						
						-- Examples for 'Array maxLINQ'
						
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.First(\"o\")		--> 5
							qarray = _From \"Hello World\"; qarray.First(\"O\")		--> 0
							qarray = _From \"Hello World\"; qarray.First#(\"O\",\"ci:\")	--> 5
					
	")
					"last": helpmsg =(		--	313
	"
	Last LA : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Last(<BooleanLambdaExpression var>): Selects the last array element that satisfy the Boolean Lambda Expression.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Last#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Returns 'undefined' if any element satisfy the Boolean condition
						--	Reserved word 'index' in lambda expression which holds the index of each element
						--	Accepted modifiers: s, i
						--
						-- 'String maxLINQ':
						-- (<StringQuery>[.<sequence of query or filter functions>]).Last(<stringToFind>): Returns the index of the Last occurrence of 'stringToFind' (0 if none)
						-- (<StringQuery>[.<sequence of query or filter functions>]).Last#(<stringToFind>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
						
						-- Examples for 'Array maxLINQ'
						
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.Last(\"o\")		--> 8
							qarray = _From \"Hello World\"; qarray.Last(\"O\")		--> 0
							qarray = _From \"Hello World\"; qarray.Last#(\"O\",\"ci:\")	--> 8
			
	")
					"contains": helpmsg =(		--	314
	"
	Contains someValue : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Contains(someValue): Determines whether a sequence contains a specified value. Returns False or True
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Contains#(someValue, \"i:\"): Same as above but returns 0 if false or the index of the found element if true. 
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Contains#(someValue, \"s:\"): Same as above but searchs the value in the subarray elements. Returns an array of booleans values (or indices with \"i:\" modifier), one for each subarray.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Contains#(someValue, \"ci:\"): forces a case insensitive comparison for strings
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Contains#(someValue, <LambdaExpression var>):  Determines the equality of 'someValue' with the array elements after apllying the Lambda Expression to each element of the array. Doesn't change array Values
						--	Accepted modifiers: s, ci, i
						--	'someValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--	When the \"i:\" modifier is set, the function returns the index of the first coincidence found.
						--
						-- 'String maxLINQ':
						-- <StringQuery>[.<sequence of query or filter functions>]).Contains(<stringToFind>): Determines whether a string contains a specified string 'stringToFind'. Returns False or True
						-- <StringQuery>[.<sequence of query or filter functions>]).Contains#(<stringToFind>, \"ci:\"): same as above using case insensitive comparison
						-- <StringQuery>[.<sequence of query or filter functions>]).Contains#(<stringToFind>, \"i:\"): same as above but returns the index of the first occurrence (0 if none)
						-- Accepted modifiers: ci, i
						
						-- Examples for 'Array maxLINQ'
						
						-- Examples for 'String maxLINQ'
							qarray = _From \"Hello World\"; qarray.Contains(\"w\")		--> False
							qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"ci:\")		--> True
							qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"i,ci:\")		--> 7
							qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"i:\")		--> 0
					
	")
					"sequenceequal": helpmsg =(		--	315
	"
	SequenceEqual otherArray : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).SequenceEqual(otherArray): Determines whether two sequences are equal by comparing the elements.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).SequenceEqual#(otherArray, <LambdaExpression var>): Determines whether two sequences are equal by comparing the elements after applying the Lambda expression to each element of both arrays. Changes array Values
						--	Reserved Lambda expressions (\"s:\"), for fast calculation in case of subArray modifier without a Lambda Expression Value Comparison
						--	Accepted modifiers: s.
						--  The Default Equality is used for array elements. Returns TRUE if both arrays are equal, FALSE otherwise (or a boolean array in case of 's:' modifier and same arrays size)
						--
						-- 'String maxLINQ':
						-- <StringQuery>[.<sequence of query or filter functions>]).SequenceEqual(<otherString>): Determines whether two strings are equal. Returns False or True
						-- <StringQuery>[.<sequence of query or filter functions>]).SequenceEqual#(<otherString>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(#(10, 20), #(30, 40));
						
							qarray.sequenceEqual#(#(10,20),#(30,40))	--> True
							qarray.sequenceEqual#(#(10,20),#(30,41))	--> False
							qarray.sequenceEqual#(#(#(11,21),#(31,41)),#'x=>#(x[1]/10,x[2]/10)')	-->	True
							qarray.sequenceEqual#(#(#(11,20),#(30,40)),#'s:')	-->	#(false, true)
						
						-- Examples for 'String maxLINQ'
						
	")
					"isempty": helpmsg =(		--	316
	"
	isEmpty LA : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).isEmpty#(): Determines if the array is empty.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).isEmpty(\"s:\"): Determines if the subarray elements of the array are empty.
						--	Accepted modifiers: s.
						--	Returns TRUE if the array is empty, FALSE otherwise (or a boolean array in case of 's:' modifier)
						--
						-- 'String maxLINQ':
						-- <StringQuery>[. <sequence of query or filter functions>]).isEmpty#(): Determines whether the string is void (equal to \"\"). Returns False or True
						-- Accepted modifiers: none
						
						-- Examples for 'Array maxLINQ'
							qarray = _From #(); qarray.isempty#()							--> True
							qarray = _From #(1); qarray.isempty#()						--> False
							qarray = _From #(#(),#()); qarray.isempty#()					--> False
							qarray = _From #(#(),#()); qarray.isempty(#'s:')				--> #(true, true)
							qarray = _From #(#(),#()); (qarray.flatten(#'q:')).isempty#()	--> True
							qarray = _From #($); if ((qarray.mappedProp(#'q:modifiers mod=>isKindOf mod Bend')).flatten(#'q:')).isEmpty#() do (addModifier $ (Bend()))	--> add Bend modifier if not present
						
						-- Examples for 'String maxLINQ'
						
	")
					"compare": helpmsg =(		--	317
	"
	 Compare otherArray : Final Function 
						--	(<ArrayQuery>[. <sequence of query or filter functions>]).Compare#(otherArray, <LambdaExpression var>): Compares two arrays by comparing the result after applying the Lambda expression to each array. The result must be a comparable value.
						--	For subarray calculations, compares each subarray with 'otherArray' (after applying the Lambda expression to each) -i.e. comparison is allways against the same value-
						--	Returns a positive value if original array is greater than otherArray, negative value if lesser or 0 if equal.
						--	Reserved Lambda expressions: none
						--	Accepted modifiers: s, ci
						--
						-- 'String maxLINQ':
						-- <StringQuery>[. <sequence of query or filter functions>]).Compare(<otherString>): Compares two strings. Returns positive value if greater, negative if lesser or 0 if equal.
						-- <StringQuery>[. <sequence of query or filter functions>]).SequenceEqual#(<otherString>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
						
						-- Examples for 'Array maxLINQ'
							(_From #(3,4,5)).compare#(#(2,4,5),\"x=>x[1]\")			--> 1
							(_From #(3,4,5)).compare#(#(2,4,5),\"x=>x[2]\")			--> 0
							(_From #(3,4,5)).compare#(#(2,4,5),\"x=>(_From x).sum#()\")	--> 1
							(_From #(#(3,4,5),#(2,4,7),#(3,4,8))).compare#(#(2,4,7),\"s: x=>(_From x).sum#()\")	--> #(-1, 0, 2)
							
							(_From #(\"Hello\",\" \",\"World\")).compare#(#(\"Hello World\"),\"x => (_From x).sum#()\")		--> 0
							(_From #(\"Hello\",\" \",\"World\")).compare#(#(\"Hello \",\"world\"),\"x => (_From x).sum#()\")		--> -1
							(_From #(\"Hello\",\" \",\"World\")).compare#(#(\"Hello \",\"world\"),\"ci: x => (_From x).sum#()\")		--> 0
							qarray =_From #(#(\"Hello\",\" \",\"World\"),#(\"Hello\",\" \",\"world\")); qarray.compare#(#(\"Hello \",\"world\"),\"s: x => (_From x).sum#()\")	--> #(-1, 0)
						
						-- Examples for 'String maxLINQ'
							(_From \"Hello\").compare(\"Hello\")			--> 0
							(_From \"Hello\").compare(\"Hella\")			--> 1
							(_From \"Hello\").compare(\"Hellu\")			--> -1
							(_From \"Hello\").compare(\"hello\")			--> -1
							(_From \"Hello\").compare#(\"hello\",\"ci:\")	--> 0
	")
					"updateobjects": helpmsg =(		--	318
	"
	updateObjects voidArray: Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).updateObjects#(): updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
						--	Accepted modifiers: none
						--	Returns previous array query result.
						--
						-- 'String maxLINQ':
						-- This Function is not valid for 'String maxLINQ'. 

						-- Examples for 'Array maxLINQ'
							qarray = _From geometry;
							(((qarray._Where(#'q:node => isKindOf node Editable_Mesh')		--> select Editable_Mesh nodes...
										).mappedProp(#'q:verts vset => true')				--> collect all their vertices...
										).Apply(#'q,s:v=>v.pos += [0,0,10]')				--> move each vertex 10 units up in the Z axis...
										).UpdateObjects#()	--> move all objects		--> update their geometry.
	")

				)
				print helpmsg
				ok
			),
			
			fn helpMAN =		-- help on maxLINQ Manager Functions
			(
				helpmsg = (
	"
	MANAGER FUNCTIONS:
		maxLINQ.help(): shows the summary of functions for maxLINQ instances
		maxLINQ.helpFN \"<function name>\": shows the specific help and examples for the maxLINQ function
		maxLINQ.helpFN \"LA\": shows help for LambdExpressions syntax
		maxLINQ.helpMAN(): shows this message for help on maxLINQ Manager Functions
		maxLINQ.isInstance <qarray>: checks if <qarray> is a maxLINQ instance. 	Returns TRUE if qarray is a maxLINQ instance, FALSE otherwise.
		maxLINQ.dispose <[&]qarray>: Frees the qarray maxLINQ instance array (if qarray is a maxLINQ instance). If '&' parameter, qarray variable is set to undefined
		maxLINQ.createInstance <array>: returns a maxLINQ instance with <array> value. <array> must be an Enumerable value (array, string, dotNet Enumerable, objectSet..).
		maxLINQ.Show <qarray>: if qarray is a maxLINQ instance, returns its array value (Same as qarray.Show() method).
		
	MANAGER UTILITIES:
		maxLINQ.NRange <valuePair|valueTriple>: Generates a sequence of numbers within a specified range.
									--	maxLINQ.NRange#(int1, int2): Generates a sequence of integer numbers within a specified range (incremented by 1).
									--	maxLINQ.NRange[int1, int2]: Generates a sequence of float numbers within a specified range (incremented by 1.0).
									--	maxLINQ.NRange#(int1, int2, step): Generates a sequence of integer numbers within a specified range incremented by 'step' integer value.
									--	maxLINQ.NRange[int1, int2, step]: Generates a sequence of float numbers within a specified range incremented by 'step' float value.
								/*		
								-- Examples
									maxLINQ.NRange#(0,10)				--> #(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
									maxLINQ.NRange([0,10])				--> #(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
									maxLINQ.NRange#(0,10,2)			--> #(0, 2, 4, 6, 8, 10)
									maxLINQ.NRange([0,10,2])			--> #(0.0, 2.0, 4.0, 6.0, 8.0, 10.0)
							
								-- Examples for 'String maxLINQ'
									maxLINQ.NRange#(65,90)			--> \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"
								*/		
		maxLINQ.SRange <valuePair>: Generates a string with a sequence of especified ASCII characters
									--	maxLINQ.SRange#(int1, int2): Returns a string with the sequence of characters from ASCII int1 to ASCII int2
								/*		
								-- Examples
									maxLINQ.SRange#(65,90)			--> \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"
									maxLINQ.SRange#(48,57)			--> \"0123456789\"
								*/		
		maxLINQ.Repeat <valuePair>: Generates a sequence that contains one repeated value a specified number of times.
									--	maxLINQ.Repeat#(value, times): Generates an array that contains one repeated 'value' a specified number of 'times'. 'value' can be anything but a string
									--	maxLINQ.Repeat#(stringValue, times): Returns a string with the 'stringValue' repeated a specified number of times.
								/*
								-- Examples 
									maxLINQ.repeat#(0,10)					--> #(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
									maxLINQ.repeat#(\"Aa\",10)			--> \"AaAaAaAaAaAaAaAaAaAa\"
								*/
		maxLINQ.maxInstances MAXClassType: Returns a maxLINQ instance whose value is an array with all the MaxClassType instances in the scene.
								/*
								-- Examples 
									maxLINQ.maxInstances Sphere		--> (maxLINQstruct ResultArray:#(Sphere, Sphere, Sphere, Sphere)) for a scene with 4 spheres.
								*/
		maxLINQ.readFile filename: Returns a maxLINQ instance whose value is a string array where each element corresponds to a line of the specified File
								/*
								-- Examples 
									maxLINQ.readFile \"coords.txt\"		--> (maxLINQstruct ResultArray:#(\"10.5, 3.7, 0.0\", \"13.7, 10.8, 1.0\", \"18.9, 5.2, 1.0\", \"18.9, 3.7, 0.0\"))
								*/
		maxLINQ.HWND word: Returns all the child windows which name contains the given word. The search is case insensitive
								/*
								-- Examples 
									maxLINQ.HWND \"camera\"			--> #(#(2295788P, 2689080P, 2689080P, \"Button\", \"Cameras\", 0P, 2951896P, 2951896P))
								*/
		maxLINQ.toArray enumerable: Returns the enumerable collection as array (valid for ObjectSet, String, Name, PathName, bitArray, ArrayParameter, SelectionSet, dotNet Enumerables...)
												-- For isolated objects, it returns an array with the object and all its children and subchildren
												-- For strings and names, it returns an array with splited characters from the string or name
												-- For not enumerable values, returns a singleton array containing the value
								/*
								-- Examples 
									maxLINQ.toArray \"Hello World\"		--> #(\"H\", \"e\", \"l\", \"l\", \"o\", \" \", \"W\", \"o\", \"r\", \"l\", \"d\")
									maxLINQ.toArray #{1..10}		--> #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
									
									var = dotNetObject \"System.Collections.Generic.List`1[System.Int32]\"; for i = 1 to 10 do var.add i;
									maxLINQ.toArray var		--> #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
									
									maxLINQ.toArray objects	--> array containing all scene objects	
									maxLINQ.toArray $				--> array containing selected node and all its children and subchildren (for singleton selection)	
								*/
		maxLINQ.toLookUp Key: Value: Count: : Creates a 'LookUp' struct whith the optionals input 'key', 'value' and 'dimension'. If 'Value' is supplied, 'Count' is initialized to the Value dimension if not supplied.
								/*
								-- Examples 
									maxLINQ.toLookUp key:\"selected\" value:#{2..5,10..13,20}	--> (lookUp key:\"selected\" value:#{2..5, 10..13, 20} count:9)
								*/
		maxLINQ.toSelSet #(<object>, <Property>): Creates a 'SelSet' struct for querying geometries of Editable_Meshes, Editable_Polys and Trimeshes
																		-- Valid <Property> are: \"v\" or 1 (verts), \"f\" or 2 (faces), \"e\" or 3 (edges), \"sv\" or 4 (selectedVerts), \"sf\" or 5 (selectedFaces), \"se\" or 6 ( (selected edges)
								/*
								-- Examples 
									maxLINQ.toSelSet #($Sphere001, \"sv\")	--> (selSet object:$Editable_Mesh:Sphere001 @ [30.512329,-79.657318,7.580048] type:\"sv\" index:#{12, 177, 235} count:3)
								*/
	")		
				print helpmsg
				ok
			),
			-------------------------
			--	#1[End of Label 1: can be deleted for ditribution from the label above]
			-------------------------
			
			fn isInstance qarray =	-- 	Returns TRUE if qarray is a maxLINQ instance
			(
				isKindOf qarray maxLINQstruct
			),
			fn dispose &qarray =	-- Frees the qarray maxLINQ instance array (if qarray is a maxLINQ instance). If '&' parameter, qarray variable is set to undefined
			(
				isOK = isInstance qarray
				if isOK then
				(
					free qarray.resultArray
					try (qarray = undefined) catch()
				)
				else
				(
					format "This variable isn't a maxLINQ instance. Can't dispose it\n"
				)
				isOK
			),
			fn createInstance var =	--	returns a maxLINQ instance with 'var' value. 
			(
				_From var
			),
			fn Show qarray =	--	if qarray is a maxLINQ instance, returns its array value (Same as qarray.Show() method).
			(
				isOK = isInstance qarray
				if isOK then
				(
					qarray.resultArray
				)
				else
				(
					format "This variable isn't a maxLINQ instance.\n"
				)
			),
			
			-- maxLINQ UTILITIES
			fn NRange valuePair =	--	Generates a sequence of numbers within a specified range.
											--	maxLINQ.NRange#(int1, int2): Generates a sequence of integer numbers within a specified range (incremented by 1).
											--	maxLINQ.NRange[int1, int2]: Generates a sequence of float numbers within a specified range (incremented by 1.0).
											--	maxLINQ.NRange#(int1, int2, step): Generates a sequence of integer numbers within a specified range incremented by 'step' integer value.
											--	maxLINQ.NRange[int1, int2, step]: Generates a sequence of float numbers within a specified range incremented by 'step' float value.
			(
				inputClass = classOf valuePair
				option = 0
				case inputClass of
						(
							Point2: 	(option = 2)
							Point3: 	(option = 3; if valuePair[3] == 0 do valuePair[3] = 1)
							Array: 	(option = valuePair.count; valuePair = for i in valuePair collect (i as integer); if option == 3 and valuePair[3] == 0 do valuePair[3] = 1)
						)
				if option == 2 then
				(
					Result = for i = valuePair[1] to valuePair[2] collect i
				)
				else
				(
					Result = for i = valuePair[1] to valuePair[2] by valuePair[3] collect i
				)
				
				return (Result)
			),
			fn SRange valuePair =	--	Generates a string with a sequence of especified ASCII characters
											--	maxLINQ.SRange#(int1, int2): Returns a string with the sequence of characters from ASCII int1 to ASCII int2
			(
				ResultString = ""
				for i = valuePair[1] to valuePair[2] do ResultString += bit.intAsChar i
				return ResultString
			),
			fn Repeat valuePair =	--	Generates a sequence that contains one repeated value a specified number of times.
											--	maxLINQ.Repeat#(value, times): Generates an array that contains one repeated 'value' a specified number of 'times'. 'value' can be anything but a string
											--	maxLINQ.Repeat#(stringValue, times): Returns a string with the 'stringValue' repeated a specified number of times.
			(
				if (isKindOf valuePair[1] String) do
				(
					ResultString = ""
					for i = 1 to valuePair[2] do ResultString += valuePair[1]
					return ResultString
				)
				
				Result = for i = 1 to valuePair[2] collect valuePair[1]
				return (Result)
			),
			fn maxInstances MAXClassType =	--	maxLINQ.maxInstances <MAXClassType>: Returns a maxLINQ instance whose value is an array with all the MaxClassType instances in the scene.
			(
				Result = _From (getClassInstances MAXClassType)
			),
			fn readFile filename =	--	maxLINQ.readFile filename: Returns a maxLINQ instance whose value is a string array where each element corresponds to a line of the specified File
			(
				Result = _From ((dotnetclass "System.IO.File").ReadAllLines filename)
			),
			fn HWND word =	-- maxLINQ.HWND word: Returns all the child windows which name contains the given word. The search is case insensitive
			(
				qarray = _From (windows.getChildrenHWND #max)
				qarray._Where#(word,#'child word => (_From child[5]).Contains#(word,"ci:") and not (_From child[5]).Contains#("maxLINQ")')
			),
			fn toArray enumerable =	-- maxLINQ.toArray enumerable: Returns the enumerable collection as array (valid for ObjectSet, String, Name, PathName, bitArray, ArrayParameter, SelectionSet, dotNet Enumerables...)
												-- For isolated objects, it returns an array with the object and all its children and subchildren
												-- For strings and names, it returns an array with splited characters from the string or name
												-- For not enumerable values, returns a singleton array containing the value
			(
				Result =
				case of
				(
					(isKindOf enumerable Array): enumerable
					(isKindOf enumerable Node): join #() enumerable
					(isKindOf enumerable ObjectSet): enumerable as array
					(isKindOf enumerable String): (dotnet.ValueToDotNetObject enumerable dotNetString).ToCharArray()
					(isKindOf enumerable Name): (dotnet.ValueToDotNetObject (enumerable as string) dotNetString).ToCharArray()
					(isKindOf enumerable PathName): enumerable as array
					(isKindOf enumerable bitArray): enumerable as array
					(isKindOf enumerable ArrayParameter): enumerable as array
					(isKindOf enumerable dotNetObject): 	(
																			hasItemProp = (try (enumerable.item[0]; true) catch (false))	--	(enumerable.GetType()).GetProperty "Item" is ambiguous in most cases and throws an error
																			if hasItemProp then
																			(
																				for i = 0 to (enumerable.count-1) collect (enumerable.Item[i])
																			)
																			else
																			(
																				isIEnumerable = maxLINQ.IndexSort.isIEnumerable enumerable
																				if isIEnumerable then
																				(
																					iterator = enumerable.getEnumerator(); i = 0; prop = #()
																					while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; append prop iterValue)
																					prop
																				)
																				else
																				(
																					#(enumerable)
																				)
																			)
																		)																			
					default: (try (join #() enumerable) catch (#(enumerable)))
				)
			),
			fn toLookUp Key: Value: Count: =	-- maxLINQ.toLookUp Key: Value: Count: : Creates a 'LookUp' struct whith the optionals input 'key', 'value' and 'dimension'. If 'Value' is supplied, 'Count' is initialized to the Value dimension.
			(
				resultLookUp = lookUp Key Value Count
				if Value != unsupplied and Count == unsupplied do
				(
					try
					(
						resultLookUp.Count = if isKindOf Value bitArray then Value.numberSet else Value.count 
					)
					catch
					(
						resultLookUp.Count = 1
					)
				)
				return resultLookUp
			),
			fn toSelSet valuePair =		-- maxLINQ.toSelSet #(<object>, <Property>): Creates a 'SelSet' struct for querying geometries of Editable_Meshes, Editable_Polys and Trimeshes
												-- Valid <Property> are: "v" (verts), "f" (faces), "e" (edges), "sv" (selectedVerts), "sf" (selectedFaces), "se" ( (selected edges)
			(
				obj = valuePair[1]
				prop = toLower valuePair[2] 
				if (isKindOf obj Editable_Mesh) or (isKindOf obj Editable_Poly) or (isKindOf obj Trimesh) then
				(
					resultSelSet =
					case prop of
					(
						("v"): 	selSet obj prop (sel = obj.verts as bitArray) sel.numberSet
						("f"): 	selSet obj prop (sel = obj.faces as bitArray) sel.numberSet
						("e"): 	selSet obj prop (sel = obj.edges as bitArray) sel.numberSet
						("sv"): 	selSet obj prop (sel = obj.selectedVerts as bitArray) sel.numberSet
						("sf"):	selSet obj prop (sel = obj.selectedFaces as bitArray) sel.numberSet
						("se"):	selSet obj prop (sel = obj.selectedEdges as bitArray) sel.numberSet
						default: false
					)
					return resultSelSet
				)
				else
				(
					return false
				)
			),
			
			on create do
			(
				IndexSort = (compileIndexSort()).CreateInstance "PathScripts.IndexSort"
			)
		)
		::maxLINQ = maxLINQManager()
	)
	
OK	
)	
	


			