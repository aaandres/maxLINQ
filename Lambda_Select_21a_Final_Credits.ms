
(
	----------------------------------------------
	----------------------------------------------
	/*
	
	
	----------------------
	For highlighting main commands in your maxScript editor, copy (or add commands) the following lines to your MXS_EditorUser.properties :
	
		# keywords12 is for user defined words to be colorized. Keywords are checked for in this list first
		keywords12.$(file.patterns.MAXScript)=\
		_from maxlinq maxlinqman maxlinqman.help maxlinqman.helpfn maxlinqman.helpman maxlinqman.check maxlinqman.isinstance maxlinqman.dispose maxlinqman.createinstance maxlinqman.toarray

		# Keywords12 - User defined
		style.MAXScript.23=fore:#DA5937,bold
	
	----------------------
	*/
	-- Struct for Querying arrays.
	-- Just valid for one multiquerying on the given array as the input array is not kept in the structure.
	
	global maxLINQMan = if ::maxLINQMan != undefined do ::maxLINQMan
	global maxLINQ = if ::maxLINQ != undefined do ::maxLINQ
	global P3D_FileAssemblyIndexSort = if ::P3D_FileAssemblyIndexSort != undefined do ::P3D_FileAssemblyIndexSort
	global P3D_ArrayQueryLambdaFN
	global _From = ::maxLINQ

	--valid--if ::maxLINQ == undefined do
	--valid--(
		struct maxLINQ
		(
			public
			ResultArray,
			
			private
			P3D_objMaxLinq,
			-- Working parameters and functions
			vars = "",
			fnExpression = "",
			finalQuery = true,
			subArrayCalc = false,
			indexCalc = false,
			useIndex = false,
			useObject = false,
			useKey = false,
			useDescending = false,
			caseSensitive = true,
			IndexSort = ::P3D_FileAssemblyIndexSort.CreateInstance "PathScripts.IndexSort",
			dotNetString = dotNetClass "System.String",
			dotNetStringComparer = dotNetClass "System.StringComparison",
			strCompCI = dotNetStringComparer.OrdinalIgnoreCase,
			
			fn initializeModifiers =
			(
				finalQuery = true	
				subArrayCalc = false
				indexCalc = false
				useIndex = false
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
							"index": useIndex = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
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
							"index": useIndex = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
				return soft
			),
			fn Equals x y result:on = if not result then result else	--	Function by DenisT. Equality of arrays
			(
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
					(::P3D_ArrayQueryLambdaFN x < ::P3D_ArrayQueryLambdaFN y): (-dir)
					(::P3D_ArrayQueryLambdaFN x > ::P3D_ArrayQueryLambdaFN y): dir
					default: 0
				)
			),
			fn caseInsensitiveComparer x y DES:useDescending =
			(
				dir = if DES then -1 else 1
				(stricmp (::P3D_ArrayQueryLambdaFN x) (::P3D_ArrayQueryLambdaFN y)) * dir
			),
			
			------------------------------------------------------------------------------------------------------
			-- Lambda Expressions Syntax: (valid as names #' ' and strings " ")
			-- (#'[q,s,i,index,obj,key:] var => <var expression>')  : applies <var expression> to each 'var' element of the array
			-- #(extVar, #'[q,s,i,index,obj,key:] var extVar => <var and extVar expression')  : applies <var and extVar expression> to each 'var' element of the array where 'extVar' is an external variable
			-- Modifiers [q,s,i,index,obj,key:]:
				-- Syntax: separated by comas (',') if more than one and ended by colon (':')
				-- q: indicates that it's not the last function in the query. Thus, the returned value will be a struct, not an array.
				-- s: works in subarray elements if the array is a multiarray (valid in most query functions)
				-- i: returns indices of elements instead of the elements value (valid in most query functions)
				-- index: allows to use the variable 'index' in the lambda expression which holds the index of the element (valid in some query functions)
				-- obj: only valid in the 'mappedProperty', 'mappedGeometry', '_Where' and 'Select' functions. Allows to use the variable 'obj' in the lambda expression which holds the maxObject element in the array or of a previous query in '_Where' and 'Select' cases.
				-- key: only valid in the 'mappedProperty', 'mappedGeometry' and 'GroupBy' functions. The result query will be an array of structs with 3 parameters: key, value and count (see each case in their respective help).
				-- d: only valid in the 'Sort' query function. The sort will be done in descending order
				-- ci: The comparisons will be case Insensitive for strings. NOTE: all functions are CaseSensitive by default(valid in some query functions)
			------------------------------------------------------------------------------------------------------
			
			public
			-- QUERY FUNCTIONS (Use a Boolean Lambda Expression. Don't affect to array elements values. Allow further queries)
			fn _Where LA =	--	<ArrayQuery>._Where(<BooleanLambdaExpression var>): Selects the array elements that satisfy the Boolean Lambda Expression.
									--	<ArrayQuery>._Where#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
									--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
									--	Accepted modifiers: q, s, i, obj, index
									--
									-- 	'String maxLINQ':
									--	<StringQuery>._Where(<stringToSearch>): Returns an array with the indices of all the matches of the 'stringToSearch' in the original string
									-- 	<StringQuery>._Where#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
									--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if useObject then	
				(
					if not subArrayCalc do P3D_objMaxLinq_temp = #()
					if external then
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id] ext) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; ResultArray[id])
								(subArrayCalc and not indexCalc):			ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id] ext) collect ResultArray[i][id])
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id] ext) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; id)	
								(subArrayCalc and indexCalc): 				ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id] ext) collect id)
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] ResultArray[id] ext) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; ResultArray[id])
								(subArrayCalc and not indexCalc):			ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] ResultArray[i][id] ext) collect ResultArray[i][id])
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] ResultArray[id] ext) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; id)	
								(subArrayCalc and indexCalc): 				ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] ResultArray[i][id] ext) collect id)
							)
						)
					)
					else
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; ResultArray[id])
								(subArrayCalc and not indexCalc):			ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id]) collect ResultArray[i][id])
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] id ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; id)	
								(subArrayCalc and indexCalc): 				ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] id ResultArray[i][id]) collect id)
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; ResultArray[id])
								(subArrayCalc and not indexCalc):			ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] ResultArray[i][id]) collect ResultArray[i][id])
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA P3D_objMaxLinq[id] ResultArray[id]) collect (append P3D_objMaxLinq_temp P3D_objMaxLinq[id]; id)	
								(subArrayCalc and indexCalc): 				ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count where (LA P3D_objMaxLinq[i] ResultArray[i][id]) collect id)
							)
						)
					)
					if not subArrayCalc do P3D_objMaxLinq = P3D_objMaxLinq_temp
				)
				else
				(
					if external then
					(
						if useIndex then
						(
							LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id] ext) collect ResultArray[id]	--	normal
								(subArrayCalc and not indexCalc):			ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id] ext) collect Item[id]) -- subarray
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id] ext) collect id	-- index	
								(subArrayCalc and indexCalc): 				ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id] ext) collect id) -- subarray+index
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for item in ResultArray where (LA item ext) collect item	--	normal
								(subArrayCalc and not indexCalc):			ResultArray = for item in ResultArray collect (for subItem in item where (LA subItem ext) collect subItem) -- subarray
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA ResultArray[id] ext) collect id	-- index	
								(subArrayCalc and indexCalc): 				ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA Item[id] ext) collect id) -- subarray+index
							)
						)
					)
					else
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id]) collect ResultArray[id]	--	normal
								(subArrayCalc and not indexCalc):			ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id])  collect Item[id]) -- subarray
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA id ResultArray[id]) collect id	-- index	
								(subArrayCalc and indexCalc): 				ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA id Item[id]) collect id) -- subarray+index
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
							case of
							(
								(not subArrayCalc and not indexCalc):	ResultArray = for item in ResultArray where (LA item) collect item	--	normal
								(subArrayCalc and not indexCalc):			ResultArray = for item in ResultArray collect (for subItem in item where (LA subItem) collect subItem) -- subarray
								(not subArrayCalc and indexCalc):			ResultArray = for id = 1 to ResultArray.count where (LA ResultArray[id]) collect id	-- index	
								(subArrayCalc and indexCalc): 				ResultArray = for item in ResultArray collect (for id = 1 to item.count where (LA Item[id]) collect id) -- subarray+index
							)
						)
					)
				)

				return (if finalQuery then ResultArray else this)
			),
			fn W LA = _Where LA,		--	Same as _Where
			fn mappedProperty LA =	--	<ArrayQuery>.mappedProperty(<BooleanLambdaExpression property var>): Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the  Boolean Lambda Expression.
												--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the original maxObject_element
												--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																			--	Key: holds the MaxObject
																			--	Value: holds the array with the result properties of the query for the key MaxObject
																			--	Count: the number of elements in the array of properties for this MAxObject
												--	Some mapped Properties: verts, faces, edges, selectedVerts, selectedFaces, selectedEdges, children, modifiers, controller.Keys...
												--	Accepted modifiers: q, i, obj
												--	Allways returns a multisubarray with each subArray corresponding to the catched properties of each MaxObject in the original array
												--
												-- 'String maxLINQ':
												-- This Function is not valid for 'String MaxLINQ'. 
			(
				P3D_objMaxLinq = if isKindOf ResultArray Array then deepCopy ResultArray else ResultArray
				parser LA
				vars = (filterString vars " \t")[1]
				fnExpression = substring fnExpression (vars.count+2) -1
				
				if useKey then
				(
					struct lookUp (Key, Value, Count)
					ResultStructArray = for o in ResultArray collect (ResultStruct = lookUp(); ResultStruct.Key = o; ResultStruct)
				)
				
				if useObject then
				(
					LA =  execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
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
				)
				else
				(
					LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
					case of
					(
						(not indexCalc):	
							(
								ResultArray = for o in ResultArray collect (prop = join #() (getProperty o vars); for p in prop where (LA p) collect (p))
							)
						(indexCalc):
							(
								ResultArray = for o in ResultArray collect (prop = join #() (getProperty o vars); for p = 1 to prop.count where (LA prop[p]) collect (p))
							)
					)
				)
				
				if useKey then
				(
					ResultArray = for i = 1 to ResultArray.count collect (ResultStructArray[i].Value = ResultArray[i]; ResultStructArray[i].Count = ResultArray[i].count; ResultStructArray[i]) 
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn mappedProp LA = mappedProperty LA,		--	Same as mappedProperty
			fn mappedGeometry LA =	--	<ArrayQuery>.mappedGeometry(<BooleanLambdaExpression GeometyProperty var>): Collects the  'Geometry' of the Geometry_Objects in the array that satisfy the  Boolean Lambda Expression.
												--	Only valid for Verts, Faces and Edges and their 'Selected' ones, for Editable_Meshes and Editable_Poly
												--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the original maxObject_element
												--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
																			--	Key: holds the geometry object
																			--	Value: holds the array with the result of the query for the key object
																			--	Count: the number of elements in the array
												--	Accepted modifiers: q, i, key, obj
												--	Allways returns a multisubarray with each subArray corresponding to the catched geometry of each MaxObject in the original array
												--
												-- 'String maxLINQ':
												-- This Function is not valid for 'String MaxLINQ'. 
			(
				P3D_objMaxLinq = if isKindOf ResultArray Array then deepCopy ResultArray else ResultArray
				parser LA
				vars = (filterString vars " \t")[1]
				fnExpression = substring fnExpression (vars.count+2) -1
				
				if useKey then
				(
					struct lookUp (Key, Value, Count)
					ResultStructArray = for o in ResultArray collect (ResultStruct = lookUp(); ResultStruct.Key = o; ResultStruct)
				)
				
				if useObject then
				(
					LA =  execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
					case of
					(
						(not indexCalc):		--	normal
							(
								case vars of
								(
									"verts": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numVerts where (LA o (item = getVert o i)) collect item)
																								else (for i = 1 to o.numVerts where (LA o (item = polyop.getVert o i)) collect item))
									"faces": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numFaces where (LA o (item = getFace o i)) collect item)
																								else (for i = 1 to o.numFaces where (LA o (item = polyop.getFaceVerts o i)) collect item))
									"edges": 			ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for ed in o.Edges where (LA o ed) collect ed)
																								else (for i = 1 to (polyop.getnumedges o) where (LA o (item = polyop.getEdgeVerts o i)) collect item))
									"selectedverts": 	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA o (item = getVert o i)) collect item)
																								else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA o (item = polyop.getVert o i)) collect item))
									"selectedfaces":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA o (item = getFace o i)) collect item)
																								else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA o (item = polyop.getFaceVerts o i)) collect item))
									"selectededges":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA o (item = o.Edges[i])) collect item)
																								else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA o (item = polyop.getEdgeVerts o i)) collect item))
								)
							)
						(indexCalc):			-- index
							(
								case vars of
								(
									"verts": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numVerts where (LA o (getVert o i)) collect i)
																								else (for i = 1 to o.numVerts where (LA o (polyop.getVert o i)) collect i))
									"faces": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numFaces where (LA o (getFace o i)) collect i)
																								else (for i = 1 to o.numFaces where (LA o (polyop.getFaceVerts o i)) collect i))
									"edges": 			ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.Edges.count where (LA o (o.Edges[i])) collect i)
																								else (for i = 1 to (polyop.getnumedges o) where (LA o (polyop.getEdgeVerts o i)) collect i))
									"selectedverts": 	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA o (getVert o i)) collect i)
																								else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA o (polyop.getVert o i)) collect i))
									"selectedfaces":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA o (getFace o i)) collect i)
																								else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA o (polyop.getFaceVerts o i)) collect i))
									"selectededges":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA o (o.Edges[i])) collect i)
																								else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA o (polyop.getEdgeVerts o i)) collect i))
								)
							)
					)
				)
				else
				(
					LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
					case of
					(
						(not indexCalc):		--	normal
							(
								case vars of
								(
									"verts": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numVerts where (LA (item = getVert o i)) collect item)
																								else (for i = 1 to o.numVerts where (LA (item = polyop.getVert o i)) collect item))
									"faces": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numFaces where (LA (item = getFace o i)) collect item)
																								else (for i = 1 to o.numFaces where (LA (item = polyop.getFaceVerts o i)) collect item))
									"edges": 			ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for ed in o.Edges where (LA ed) collect ed)
																								else (for i = 1 to (polyop.getnumedges o) where (LA (item = polyop.getEdgeVerts o i)) collect item))
									"selectedverts": 	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA (item = getVert o i)) collect item)
																								else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA (item = polyop.getVert o i)) collect item))
									"selectedfaces":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA (item = getFace o i)) collect item)
																								else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA (item = polyop.getFaceVerts o i)) collect item))
									"selectededges":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA (item = o.Edges[i])) collect item)
																								else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA (item = polyop.getEdgeVerts o i)) collect item))
								)
							)
						(indexCalc):			-- index
							(
								case vars of
								(
									"verts": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numVerts where (LA (getVert o i)) collect i)
																								else (for i = 1 to o.numVerts where (LA (polyop.getVert o i)) collect i))
									"faces": 				ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.numFaces where (LA (getFace o i)) collect i)
																								else (for i = 1 to o.numFaces where (LA (polyop.getFaceVerts o i)) collect i))
									"edges": 			ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (for i = 1 to o.Edges.count where (LA (o.Edges[i])) collect i)
																								else (for i = 1 to (polyop.getnumedges o) where (LA (polyop.getEdgeVerts o i)) collect i))
									"selectedverts": 	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedVerts as bitarray) as array; for i in indexArray where (LA (getVert o i)) collect i)
																								else (indexArray = (polyop.getVertSelection o) as array; for i in indexArray where (LA (polyop.getVert o i)) collect i))
									"selectedfaces":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedFaces as bitarray) as array; for i in indexArray where (LA (getFace o i)) collect i)
																								else (indexArray = (polyop.getFaceSelection o) as array; for i in indexArray where (LA (polyop.getFaceVerts o i)) collect i))
									"selectededges":	ResultArray = for o in ResultArray collect (
															if iskindof o Editable_mesh then (indexArray = (o.selectedEdgess as bitarray) as array; for i in indexArray where (LA (o.Edges[i])) collect i)
																								else (indexArray = (polyop.getEdgeSelection o) as array; for i in indexArray where (LA (polyop.getEdgeVerts o i)) collect i))
								)
							)
					)
				)
				
				if useKey then
				(
					ResultArray = for i = 1 to ResultArray.count collect (ResultStructArray[i].Value = ResultArray[i]; ResultStructArray[i].Count = ResultArray[i].count; ResultStructArray[i]) 
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn mappedGeom LA = mappedGeometry LA,		--	Same as mappedGeometry
			fn DotNetIterator LA =		--	<ArrayQuery>.DotNetIterator(<BooleanLambdaExpression CollectionItem>): Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the  Boolean Lambda Expression.
												--	Not Reserved words in lambda expression
												--	Some Dotnet Iteratable: List, "System.Management.ManagementClass" instances ...
												--	Accepted modifiers: q, s, i
												--
												-- 'String maxLINQ':
												-- This Function is not valid for 'String MaxLINQ'. 
			(
				parser LA
				LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
				
				case of
				(
					(not subArrayCalc and not indexCalc):		--	normal 
						(
							ResultArray = for o in ResultArray collect (
								prop = #(); iterator = o.getEnumerator()
								while iterator.MoveNext() do (iterValue = iterator.Current; if (LA iterValue) do append prop iterValue)
								prop
								)
						)
					(subArrayCalc and not indexCalc):		--	subArray 			
						(
							ResultArray = for itemMap in ResultArray collect(
								for o in itemMap collect (
									prop = #(); iterator = o.getEnumerator()
									while iterator.MoveNext() do (iterValue = iterator.Current; if (LA iterValue) do append prop iterValue)
									prop
									)
							)
						)
					(not subArrayCalc and indexCalc):			-- index			
						(
							ResultArray = for o in ResultArray collect (
								prop = #(); iterator = o.getEnumerator(); i = 0
								while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA iterValue) do append prop i)
								prop
								)
						)
					(subArrayCalc and indexCalc):			-- subarray+index			
						(
							ResultArray = for itemMap in ResultArray collect(
								for o in itemMap collect (
									prop = #(); iterator = o.getEnumerator(); i = 0
									while iterator.MoveNext() do (i +=1; iterValue = iterator.Current; if (LA iterValue) do append prop i)
									prop
									)
							)
						)
				)
				
				if ResultArray.count == 1 and isKindOf ResultArray[1] Array do ResultArray = ResultArray[1]
				return (if finalQuery then ResultArray else this)
			),
			fn DotNetIteratorGet LA =		--	<ArrayQuery>.DotNetIteratorGet(<BooleanLambdaExpression CollectionItem>): Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
													--	Not Reserved words in lambda expression
													--	Some Dotnet Iteratable: List, "System.Management.ManagementClass" instances ...
													--	Accepted modifiers: q, s
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String MaxLINQ'. 
			(
				parser LA
				LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
				
				case of
				(
					(not subArrayCalc):		--	normal
						(
							ResultArray = for o in ResultArray collect (
								prop = #(); iterator = o.getEnumerator()
								while iterator.MoveNext() do (iterValue = iterator.Current; append prop (LA iterValue))
								prop
								)
						)
					(subArrayCalc):		--	subArray 
						(
							ResultArray = for itemMap in ResultArray collect(
								for o in itemMap collect (
									prop = #(); iterator = o.getEnumerator()
									while iterator.MoveNext() do (iterValue = iterator.Current; append prop (LA iterValue))
									prop
									)
							)
						)
				)
				
				if ResultArray.count == 1 and isKindOf ResultArray[1] Array do ResultArray = ResultArray[1]
				return (if finalQuery then ResultArray else this)
			),
			fn TakeWhile LA =	--	<ArrayQuery>.TakeWhile(<BooleanLambdaExpression var>): Returns the array elements as long as the Boolean Lambda Expression is true
										--	<ArrayQuery>.TakeWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: q, s, i, index
										--
										-- 	'String maxLINQ':
										--	<StringQuery>.TakeWhile(<stringToSearch>): Returns the string before the first occurrence of 'stringToSearch'
										-- 	<StringQuery>.TakeWhile#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
										--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; soft = softParser modLA; LA = LA[1])
					
					if LA != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						index = if caseSensitive then (stringDN.IndexOf LA) else (stringDN.IndexOf LA strCompCI)
						if index == -1 do index = 0
						ResultArray = subString ResultArray 1 index
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				ResultArray = 
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	for id = 1 to ResultArray.count while (LA id ResultArray[id] ext) collect ResultArray[id]	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id] ext) collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			for id = 1 to ResultArray.count while (LA id ResultArray[id] ext) collect id	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id] ext) collect id) -- subarray+index
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	for item in ResultArray while (LA item ext) collect item	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (for subItem in item while (LA subItem ext) collect subItem) -- subarray
							(not subArrayCalc and indexCalc):			for id = 1 to ResultArray.count while (LA ResultArray[id] ext) collect id	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (for id = 1 to item.count while (LA Item[id] ext) collect id) -- subarray+index
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	for id = 1 to ResultArray.count while (LA id ResultArray[id]) collect ResultArray[id]	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id])  collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			for id = 1 to ResultArray.count while (LA id ResultArray[id]) collect id	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (for id = 1 to item.count while (LA id Item[id]) collect id) -- subarray+index
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	for item in ResultArray while (LA item) collect item	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (for subItem in item while (LA subItem) collect subItem) -- subarray
							(not subArrayCalc and indexCalc):			for id = 1 to ResultArray.count while (LA ResultArray[id]) collect id	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (for id = 1 to item.count while (LA Item[id]) collect id) -- subarray+index
						)
					)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn SkipWhile LA =	--	<ArrayQuery>.SkipWhile(<BooleanLambdaExpression var>): Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
										--	<ArrayQuery>.SkipWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: q, s, i, index
										--
										-- 	'String maxLINQ':
										--	<StringQuery>.SkipWhile(<stringToSearch>): Returns the string after the first occurrence of 'stringToSearch' ('stringToSearch' included)
										-- 	<StringQuery>.SkipWhile#(<stringToSearch>, "ci:"): same as above using case insensitive comparison
										--	Accepted modifiers: ci, q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; soft = softParser modLA; LA = LA[1])
					
					if LA != "" do
					(
						stringDN = dotnet.ValueToDotNetObject ResultArray dotNetString
						index = if caseSensitive then (stringDN.IndexOf LA) else (stringDN.IndexOf LA strCompCI)
						if index != -1 do ResultArray = subString ResultArray (index+1) -1
					)
					return (if finalQuery then ResultArray else this)
				)
				
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				ResultArray = 
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id] ext) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect ResultArray[id])	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id] ext) do firstIndex = id; for id = firstIndex+1 to item.count collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id] ext) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect id)	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id] ext) do firstIndex = id; for id = firstIndex+1 to item.count collect id) -- subarray+index
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(firstIndex = 0; for id = 1 to ResultArray.count while (LA ResultArray[id] ext) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect ResultArray[id])	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA Item[id] ext) do firstIndex = id; for id = firstIndex+1 to item.count collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			(firstIndex = 0; for id = 1 to ResultArray.count while (LA ResultArray[id] ext) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect id)	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA Item[id] ext) do firstIndex = id; for id = firstIndex+1 to item.count collect id) -- subarray+index
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect ResultArray[id])	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			(firstIndex = 0; for id = 1 to ResultArray.count while (LA id ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect id)	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA id Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect id) -- subarray+index
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(firstIndex = 0; for id = 1 to ResultArray.count while (LA ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect ResultArray[id])	--	normal
							(subArrayCalc and not indexCalc):			for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect Item[id]) -- subarray
							(not subArrayCalc and indexCalc):			(firstIndex = 0; for id = 1 to ResultArray.count while (LA ResultArray[id]) do firstIndex = id; for id = firstIndex+1 to ResultArray.count collect id)	-- index	
							(subArrayCalc and indexCalc): 				for item in ResultArray collect (firstIndex = 0; for id = 1 to item.count while (LA Item[id]) do firstIndex = id; for id = firstIndex+1 to item.count collect id) -- subarray+index
						)
					)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			
			-- FILTER FUNCTIONS (Most use a Lambda Expression for projecting array values. Array values change for 'Select'=='Apply' functions. Allow further queries)
			fn Select LA =	--	<ArrayQuery>.Select(<LambdaExpression var>): Creates a new array from input array by applying Lambda Expression to array elements (the result array has the same number of elements) . Changes array values.
									--	<ArrayQuery>.Select#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
									--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
									--	Accepted modifiers: q, s, obj, index
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String MaxLINQ'. Use 'Replace' or 'ReplaceAt' functions to modify the string
			(
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				if useObject then	
				(
					if external then
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for i = 1 to ResultArray.count collect (LA P3D_objMaxLinq[i] i ResultArray[i] ext)	
								(subArrayCalc):		ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count collect (LA P3D_objMaxLinq[i] id ResultArray[i][id] ext))
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for i = 1 to ResultArray.count collect (LA P3D_objMaxLinq[i] ResultArray[i] ext)	
								(subArrayCalc):		ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count collect (LA P3D_objMaxLinq[i] ResultArray[i][id] ext))
							)
						)
					)
					else
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj index " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for i = 1 to ResultArray.count collect (LA P3D_objMaxLinq[i] i ResultArray[i])	
								(subArrayCalc):		ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count collect (LA P3D_objMaxLinq[i] id ResultArray[i][id]))
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN obj " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for i = 1 to ResultArray.count collect (LA P3D_objMaxLinq[i] ResultArray[i])	
								(subArrayCalc):		ResultArray = for i = 1 to ResultArray.count collect (for id = 1 to ResultArray[i].count collect (LA P3D_objMaxLinq[i] ResultArray[i][id]))
							)
						)
					)
				)
				else
				(
					if external then
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for id = 1 to ResultArray.count collect (LA id ResultArray[id] ext)	--	normal
								(subArrayCalc):		ResultArray = for item in ResultArray collect (for id = 1 to item.count collect (LA id Item[id] ext)) -- subarray
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for item in ResultArray collect (LA item ext)	--	normal
								(subArrayCalc):		ResultArray = for item in ResultArray collect (for subItem in item collect (LA subItem ext)) -- subarray
							)
						)
					)
					else
					(
						if useIndex then
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for id = 1 to ResultArray.count collect (LA id ResultArray[id])	--	normal
								(subArrayCalc):		ResultArray = for item in ResultArray collect (for id = 1 to item.count collect (LA id Item[id])) -- subarray
							)
						)
						else
						(
							LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
							case of
							(
								(not subArrayCalc):	ResultArray = for item in ResultArray collect (LA item)	--	normal
								(subArrayCalc):		ResultArray = for item in ResultArray collect (for subItem in item collect (LA subItem)) -- subarray
							)
						)
					)
				)
				
				
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
									-- This Function is not valid for 'String MaxLINQ'. 
			(
				parser LA
				LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)

				if useKey then
				(
					struct lookUp (Key, Value, Count)
					case of
					(
						(not subArrayCalc and not indexCalc):	--	normal
							(
								tempResult = #()
								notFound = #{1..ResultArray.count}
								for i = 1 to ResultArray.count where notFound[i] do		
								(
									ResultStruct = lookUp()
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
										ResultStruct = lookUp()
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
									ResultStruct = lookUp()
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
										ResultStruct = lookUp()
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
			fn MakeUnique LA =				--	<ArrayQuery>.MakeUnique#(): Creates a new array with unique elements. Uses 'Default Equality' function for comparing arrays
													--	<ArrayQuery>.MakeUnique(<LambdaExpression>): compares array elements after applying the Lambda expression to each element of the array
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String MaxLINQ'. 
			(
				useLA = false
				initializeModifiers()
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
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
			fn Intersect otherArray =			--	<ArrayQuery>.Intersect(otherArray): Creates a new array with the common elements of both arrays. Uses 'Default Equality' function for comparing arrays.
														--	<ArrayQuery>.Intersect#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
														--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
														--	This function doesn't "MakeUnique" result. If one item in the original array is duplicated and it exists in 'otherArray', it will be catched twice.
														--	Accepted modifiers: q, s, i
														--
														-- 'String maxLINQ':
														-- This Function is not valid for 'String MaxLINQ'. 
			(
				useLA = false
				initializeModifiers()
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
					
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
			fn Except otherArray =			--	<ArrayQuery>.Except(otherArray): Creates a new array with the elements of the original array that are not in the 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
													--	<ArrayQuery>.Except#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	This function doesn't "MakeUnique" result. If one item in the original array is duplicated and it doesn't exist in 'otherArray', it will be catched twice
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String MaxLINQ'. 
			(
				useLA = false
				initializeModifiers()
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
					
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
									Result = for j = 1 to ResultArray[i] where (findItem otherArray[i] ResultArray[i][j] == 0) collect j
								)
							)
						)
				)
				
				return (if finalQuery then ResultArray else this)
			),
			fn Union otherArray =			--	<ArrayQuery>.Union(otherArray): Creates a new array with all unique elements of original array and 'otherArray'. Uses 'Default Equality' function for comparing arrays. 
													--	<ArrayQuery>.Union#(otherArray, <LambdaExpression var>): compares array elements after applying the Lambda expression to each element of both arrays
													--	Reserved Lambda expressions ("s:"), ("i:"), ("i,s:") and ("s,i:") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
													--	This function does a "MakeUnique" result. Duplicated elements will desappear. When returning indices, the ones from the second array start at: index = (original array).count +1
													--	Accepted modifiers: q, s, i
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String MaxLINQ'. 
			(
				subArrayCalc = false
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) then
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
			fn Concat otherArray =			--	<ArrayQuery>.Concat(otherArray): Creates a new array concatenating all elements of original array and 'otherArray'. 
													--	<ArrayQuery>.Concat#(otherArray, <LambdaExpression var>): concatenates both arrays after applying the Lambda expression to each element of 'otherArray'. 
													--	Reserved Lambda expressions ("s:") - with optional 'q' modifier - for fast calculation in case of subArray modifier without a Lambda Expression Value
													--	This function doesn't "MakeUnique" result.
													--	Accepted modifiers: q, s
													--
													-- 	'String maxLINQ':
													--	<StringQuery>.Concat(<stringToAppend>): Appends 'stringToAppend' to the original string
													--	Accepted modifiers: q
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf otherArray Array) do (LA = otherArray[2]; otherArray = otherArray[1]; soft = softParser LA)
					ResultArray += otherArray
					return (if finalQuery then ResultArray else this)
				)
				
				
				useLA = false
				initializeModifiers()
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
				
				if subArrayCalc then
				(
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
									-- This Function is not valid for 'String MaxLINQ'. 
			(
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
									-- This Function is not valid for 'String MaxLINQ'. 
			(
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
									-- This Function is not valid for 'String MaxLINQ'. 
			(
				useLA = false
				initializeModifiers()
				--compileIndexSort()
				
				if not (Equals LA #()) do
				(
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
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
			fn ReOrder indexArray =		--	<ArrayQuery>.ReOrder(indexArray): Returns the array reordered based on the arrayIndex order. All the elements in array must be comparable.
													--	<ArrayQuery>.ReOrder#(indexArray, "s:"): Returns a multiarray with the reordered subarrays based on the arrayIndex subarrays order. All the elements in subArrays must be comparable
													--	indexArray must be an integer array (or multiarray) of the same length than the array to sort.
													--	Accepted modifiers: q, s
													--
													-- 'String maxLINQ':
													-- This Function is not valid for 'String MaxLINQ'. 
			(
				initializeModifiers()
				--compileIndexSort()
				
				if (indexArray.count == 2) and (isKindOf indexArray[1] Array) and ((isKindOf indexArray[2] name) or (isKindOf indexArray[2] string)) do
				(
					LA = indexArray[2]
					indexArray = indexArray[1]
					soft = softParser LA
				)
				
				case of
				(
					(not subArrayCalc):		--	normal
						(
							case (classof ResultArray[1]) of
							(
								Integer:	ResultArray = IndexSort.reOrderInt  ResultArray indexArray
								Float: 	ResultArray = IndexSort.reOrderFloat  ResultArray indexArray
								String: 	ResultArray = IndexSort.reOrderString  ResultArray indexArray
							)
						)
					(subArrayCalc):		--	subArray
						(
							ResultArray = for i = 1 to ResultArray.count collect 
								(
									case (classof ResultArray[i][1]) of
									(
										Integer:	IndexSort.reOrderInt  ResultArray[i] indexArray[i]
										Float: 	IndexSort.reOrderFloat  ResultArray[i] indexArray[i]
										String: 	IndexSort.reOrderString  ResultArray[i] indexArray[i]
									)
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				
				useLA = false
				initializeModifiers()
				if (valuePairArray.count == 3) do
				(
					LA = valuePairArray[3]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				
				useLA = false
				initializeModifiers()
				if (TriArray.count == 4) do
				(
					LA = TriArray[4]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				
				useLA = false
				initializeModifiers()
				if (isKindOf someValue Array) do
				(
					if (someValue.count == 2) and (isKindOf someValue[1] Array) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
					(
						LA = someValue[2]
						someValue = someValue[1]
						soft = softParser LA
						if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
					)
					else
					(
						if (someValue.count == 2) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
							(
								LA = someValue[2]
								soft = softParser LA
								if not soft do (try (useLA = true; soft = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)) catch (useLA = false; soft = false))
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
			fn RemoveAt valuePairArray =--	<ArrayQuery>.RemoveAt#(someValue, atIndex): Removes the occurrence of 'someValue' at 'atIndex' position if the match is true
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				
				useLA = false
				initializeModifiers()
				if (valuePairArray.count == 3) do
				(
					LA = valuePairArray[3]
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
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
									-- This Function is valid for 'String MaxLINQ'. 
			(
				var = deepCopy ResultArray
				return (this)
			),
			fn Debug stringMessage =	-- <ArrayQuery>.Debug#().<next query function>: Prints the temporary query result to Listener
												-- <ArrayQuery>.Debug(string_message).<next query function>: Prints the temporary query result to Listener preceded by the input string message
												-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
												--
												-- 'String maxLINQ':
												-- This Function is valid for 'String MaxLINQ'. 
			(
				if (Equals stringMessage #()) do stringMessage = ""
				format "%==> %\n" stringMessage ResultArray
				return (this)
			),
			
			
			-- FINAL FUNCTIONS
			fn Show =			--	(<ArrayQuery>[. <sequence of query or filter functions>]).Show(): Returns the result array after previous Query and Filter functions
									--	<ArrayQuery>.Show(): retrieves the last query as array.
									--	Only valid after a function query with 'q:' modifier or as first function to retrieve the last query.
									--
									-- 'String maxLINQ':
									-- This Function is valid for 'String MaxLINQ'. 
			(
				return ResultArray
			),
			fn ToArray = Show(),	--	Same as Show
			fn Count LA = 		--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count#(): Returns the number of elements in the result array
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count("s:"): Returns an array with the number of elements of each item in the result array
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count(<BooleanLambdaExpression var>): Returns the number of elements in the result array that satisfy the Boolean Lambda Expression.
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Count#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: s, index
										--
										-- 	'String maxLINQ':
										--	<StringQuery>[. <sequence of query or filter functions>]).Count#(): Returns the length of the string
										--	Accepted modifiers: none
			(
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
									--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
									--	Accepted modifiers: s, index
									--
									-- 'String maxLINQ':
									-- This Function is not valid for 'String MaxLINQ'. 
			(
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
			fn Average LA =		--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average#(): Returns the standard average value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average("s:"): Returns an array with the standard average of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average(<LambdaExpression var>):  Returns the Average of the array elements after applying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>]).Average#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: s, index
										--
										-- 'String maxLINQ':
										-- This Function is not valid for 'String MaxLINQ'. 
			(
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
			fn _min LA =			--	(<ArrayQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("i:"): Returns the index of the minimum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("s:"): Returns an array with the minimum value of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min("ci:"): makes a case insensitive comparison for strings
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min(<LambdaExpression var>):  Returns the minimum value of the array elements after applying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._min#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: s, i, ci, index
										--
										-- 	'String maxLINQ':
										--	(<StringQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("i:"): Returns index of the minimum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("ci:"): same as above using case insensitive comparing
										--	(<StringQuery>[. <sequence of query or filter functions>])._min("ci, i:"): same as above using case insensitive comparing and returning the index
										--	Accepted modifiers:  i, ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
			fn _max LA =			--	(<ArrayQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("i:"): Returns the index of the maximum value of the array elements
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("s:"): Returns an array with the maximum value of each subarray element
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max("ci:"): makes a case insensitive comparison for strings
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max(<LambdaExpression var>):  Returns the maximum value of the array elements after apllying the Lambda Expression to each element. Changes array Values
										--	(<ArrayQuery>[. <sequence of query or filter functions>])._max#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
										--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
										--	Accepted modifiers: s, i, ci, index
										--
										-- 	'String maxLINQ':
										--	(<StringQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("i:"): Returns index of the maximum character of the string
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("ci:"): same as above using case insensitive comparing
										--	(<StringQuery>[. <sequence of query or filter functions>])._max("ci, i:"): same as above using case insensitive comparing and returning the index
										--	Accepted modifiers:  i, ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
			fn Range valuePair =	--	<ArrayQuery>.Range#(int1, int2): Generates a sequence of integer numbers within a specified range (incremented by 1).
											--	<ArrayQuery>.Range[int1, int2]: Generates a sequence of float numbers within a specified range (incremented by 1.0).
											--	<ArrayQuery>.Range#(int1, int2, step): Generates a sequence of integer numbers within a specified range incremented by 'step' integer value.
											--	<ArrayQuery>.Range[int1, int2, step]: Generates a sequence of float numbers within a specified range incremented by 'step' float value.
											--	The original array is not affected
											--	Accepted modifiers: none
											--
											-- 	'String maxLINQ':
											--	<StringQuery>.Range#(int1, int2): Returns a string with the sequence of characters from ASCII int1 to ASCII int2
											--	Accepted modifiers: none
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					ResultString = ""
					for i = valuePair[1] to valuePair[2] do ResultString += bit.intAsChar i
					return ResultString
				)
				
				
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
			fn Repeat valuePair =	--	<ArrayQuery>.Repeat#(value, times): Generates a sequence that contains one repeated value a specified number of times.
											--	The original array is not affected
											--	Accepted modifiers: none
											--
											-- 	'String maxLINQ':
											--	<StringQuery>.Repeat#(stringValue, times): Returns a string with the 'stringValue' repeated a specified number of times.
											--	Accepted modifiers: none
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					ResultString = ""
					for i = 1 to valuePair[2] do ResultString += valuePair[1]
					return ResultString
				)
				
				Result = for i = 1 to valuePair[2] collect valuePair[1]
				return (Result)
			),
			fn All LA =		--	<ArrayQuery>.All(<BooleanLambdaExpression var>): Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
								--	<ArrayQuery>.All#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
								--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
								--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
								--	Accepted modifiers: s, index
								--
								-- 'String maxLINQ':
								-- This Function is not valid for 'String MaxLINQ'. Use 'Compare' function to query equality of strings
			(
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				resultBool = true
				resultBoolArray = #()
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc):	for id = 1 to ResultArray.count while resultBool do resultBool = (LA id ResultArray[id] ext)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = true; for id = 1 to item.count while resultBool do resultBool = (LA id Item[id] ext); resultBool)
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc):	for item in ResultArray while resultBool do resultBool = (LA item ext)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = true; for subItem in item while resultBool do resultBool = (LA subItem ext); resultBool)
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc):	for id = 1 to ResultArray.count while resultBool do resultBool = (LA id ResultArray[id])
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = true; for id = 1 to item.count while resultBool do resultBool = (LA id Item[id]); resultBool)
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc):	for item in ResultArray while resultBool do resultBool = (LA item)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = true; for subItem in item while resultBool do resultBool = (LA subItem); resultBool)
						)
					)
				)
				
				return (if subArrayCalc then resultBoolArray else resultBool)
			),
			fn Any LA =	--	<ArrayQuery>.Any(<BooleanLambdaExpression var>): Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
								--	<ArrayQuery>.Any#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
								--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
								--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
								--	Accepted modifiers: s, index
								--
								-- 'String maxLINQ':
								-- This Function is not valid for 'String MaxLINQ'. Use 'Contains' function to query if a sub_string is included in the string
			(
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				resultBool = false
				resultBoolArray = #()
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc):	for id = 1 to ResultArray.count while not resultBool do resultBool = (LA id ResultArray[id] ext)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = false; for id = 1 to item.count while not resultBool do resultBool = (LA id Item[id] ext); resultBool)
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc):	for item in ResultArray while not resultBool do resultBool = (LA item ext)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = false; for subItem in item while not resultBool do resultBool = (LA subItem ext); resultBool)
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc):	for id = 1 to ResultArray.count while not resultBool do resultBool = (LA id ResultArray[id])
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = false; for id = 1 to item.count while not resultBool do resultBool = (LA id Item[id]); resultBool)
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc):	for item in ResultArray while not resultBool do resultBool = (LA item)
							(subArrayCalc):		resultBoolArray = for item in ResultArray collect (resultBool = false; for subItem in item while not resultBool do resultBool = (LA subItem); resultBool)
						)
					)
				)
				
				return (if subArrayCalc then resultBoolArray else resultBool)
			),
			fn First LA =		--	<ArrayQuery>.First(<BooleanLambdaExpression var>): Selects the first array element that satisfy the Boolean Lambda Expression.
									--	<ArrayQuery>.First#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Returns 'undefined' if any element satisfy the Boolean condition
									--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
									--	Accepted modifiers: s, i, index
									--
									-- 'String maxLINQ':
									-- This Function has the same behavior than 'Contains' function whith the 'i:' modifier
									--	<StringQuery>[. <sequence of query or filter functions>]).First(<stringToFind>): Returns the index of the first occurrence of 'stringToFind' (0 if none)
									-- 	<StringQuery>[. <sequence of query or filter functions>]).First#(<stringToFind>, "ci:"): same as above using case insensitive comparison
									--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
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
				
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				ResultItem =
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id] ext; counter+=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id] ext; counter+=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id] ext; counter+=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id] ext; counter+=1); if found then counter else ()))
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = 0; for item in ResultArray while not found do (found = LA item ext; counter+=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = 0; for subItem in item while not found do (found = LA subItem ext; counter+=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA ResultArray[id] ext; counter+=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA Item[id] ext; counter+=1); if found then counter else ()))
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id]; counter+=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id]; counter+=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA id ResultArray[id]; counter+=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA id Item[id]; counter+=1); if found then counter else ()))
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = 0; for item in ResultArray while not found do (found = LA item; counter+=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = 0; for subItem in item while not found do (found = LA subItem; counter+=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = 0; for id = 1 to ResultArray.count while not found do (found = LA ResultArray[id]; counter+=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = 0; for id = 1 to item.count while not found do (found = LA Item[id]; counter+=1); if found then counter else ()))
						)
					)
				)
				
				return (ResultItem)
			),
			fn Last LA =		--	<ArrayQuery>.Last(<BooleanLambdaExpression var>): Selects the last array element that satisfy the Boolean Lambda Expression.
									--	<ArrayQuery>.Last#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
									--	Returns 'undefined' if any element satisfy the Boolean condition
									--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
									--	Accepted modifiers: s, i, index
									--
									-- 'String maxLINQ':
									--	<StringQuery>[. <sequence of query or filter functions>]).Last(<stringToFind>): Returns the index of the last occurrence of 'stringToFind' (0 if none)
									-- 	<StringQuery>[. <sequence of query or filter functions>]).Last#(<stringToFind>, "ci:"): same as above using case insensitive comparison
									--	Accepted modifiers: ci
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf LA Array) do (modLA = LA[2]; LA = LA[1]; soft = softParser modLA)
					
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
			
				
				if (external = isKindOf LA Array) do (ext = LA[1]; LA = LA[2])
				parser LA
				
				ResultItem =
				if external then
				(
					if useIndex then
					(
						LA =  execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id] ext; counter-=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id] ext; counter-=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id] ext; counter-=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id] ext; counter-=1); if found then counter else ()))
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA ResultArray[id] ext; counter-=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA Item[id] ext; counter-=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA ResultArray[id] ext; counter-=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA Item[id] ext; counter-=1); if found then counter else ()))
						)
					)
				)
				else
				(
					if useIndex then
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN index " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id]; counter-=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id]; counter-=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA id ResultArray[id]; counter-=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA id Item[id]; counter-=1); if found then counter else ()))
						)
					)
					else
					(
						LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)
						case of
						(
							(not subArrayCalc and not indexCalc):	(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA ResultArray[id]; counter-=1); if found then ResultArray[counter] else ())
							(subArrayCalc and not indexCalc):			(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA Item[id]; counter-=1); if found then item[counter] else ()))
							(not subArrayCalc and indexCalc):			(found = false; counter = ResultArray.count+1; for id = ResultArray.count to 1 by -1 while not found do (found = LA ResultArray[id]; counter-=1); if found then counter else ())
							(subArrayCalc and indexCalc): 				(for item in ResultArray collect (found = false; counter = item.count+1; for id = item.count to 1 by -1 while not found do (found = LA Item[id]; counter-=1); if found then counter else ()))
						)
					)
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					initializeModifiers()
					if (isKindOf someValue Array) do (LA = someValue[2]; someValue = someValue[1]; soft = softParser LA)
					
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
				
				useLA = false
				initializeModifiers()
				if (isKindOf someValue Array) do
				(
					if (someValue.count == 2) and (isKindOf someValue[1] Array) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
					(
						LA = someValue[2]
						someValue = someValue[1]
						soft = softParser LA
						if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
					)
					else
					(
						if (someValue.count == 2) and ((isKindOf someValue[2] name) or (isKindOf someValue[2] string)) then
							(
								LA = someValue[2]
								soft = softParser LA
								if not soft do (try (useLA = true; soft = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression)) catch (useLA = false; soft = false))
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
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
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
				
				useLA = false
				initializeModifiers()
				if (otherArray.count == 2) and (isKindOf otherArray[1] Array) and ((isKindOf otherArray[2] name) or (isKindOf otherArray[2] string)) do
				(
					LA = otherArray[2]
					otherArray = otherArray[1]
					
					soft = softParser LA
					if not soft do (useLA = true; parser LA; LA = execute ("fn P3D_ArrayQueryLambdaFN " + fnExpression))
				)
				
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
			fn Compare otherArray = SequenceEqual otherArray,		--	Same as SequenceEqual
			fn isEmpty LA =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).isEmpty#(): Determines if the array is empty.
									--	(<ArrayQuery>[. <sequence of query or filter functions>]).isEmpty("s:"): Determines if the subarray elements of the array are empty.
									--	Accepted modifiers: s.
									--	Returns TRUE if the array is empty, FALSE otherwise (or a boolean array in case of 's:' modifier)
									--
									-- 	'String maxLINQ':
									--	<StringQuery>[. <sequence of query or filter functions>]).isEmpty#(): Determines whether the string is void (equal to ""). Returns False or True
									--	Accepted modifiers: none
			(
				if (isKindOf ResultArray String) or (isName = isKindOf ResultArray Name) do
				(
					if isName != undefined do ResultArray = ResultArray as String
					
					if (Equals LA #()) do
					(
						ResultEmpty = ResultArray == ""
					)
					return (ResultEmpty)
				)
				
				
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
			fn updateObjects =	--	(<ArrayQuery>[. <sequence of query or filter functions>]).updateObjects(): updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
										--	Accepted modifiers: none
										--	Returns previous array query result.
										--
										-- 'String maxLINQ':
										-- This Function is not valid for 'String MaxLINQ'. 
			(
				try 
				(
					P3D_objMaxLinq.count
					if (isKindOf P3D_objMaxLinq[1] GeometryClass) do (for o in P3D_objMaxLinq do update o)
				)
				catch
				(
					if (isKindOf P3D_objMaxLinq GeometryClass) do (allchild = join #() P3D_objMaxLinq; for o in allchild do update o)
				)
				return ResultArray
			),
			
			
			
			on create do
			(
			)
		)
		::_From = maxLINQ
	--valid--)
	
	
	--valid--if ::maxLINQMan == undefined do
	--valid--(
		struct maxLINQManager
		(
			private
			vars = "",
			fnExpression = "",
			finalQuery = true,
			subArrayCalc = false,
			indexCalc = false,
			useIndex = false,
			useObject = false,
			useKey = false,
			useDescending = false,
			caseSensitive = true,
			
			fn initializeModifiers =
			(
				finalQuery = true	
				subArrayCalc = false
				indexCalc = false
				useIndex = false
				useObject = false
				useKey = false
				useDescending = false
				caseSensitive = true
			),
			fn parser LA =
			(
				LA = (LA as string)		
				assignPos = findString  LA "=>"
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
							"s": subArrayCalc = true
							"i": indexCalc = true
							"index": useIndex = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
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
							"s": subArrayCalc = true
							"i": indexCalc = true
							"index": useIndex = true
							"obj": useObject = true
							"key": useKey = true
							"d": useDescending = true
							"ci": caseSensitive = false
						)
					)
				)
				return soft
			),
			fn parityCheck theExpression theChar secondChar:"" =
			(
				theExpression = copy theExpression
				counter = 0
				posQuote = #()
				for i = 1 to theExpression.count do
				(
					pos = findstring theExpression theChar
					if pos != undefined then (theExpression[pos] = "*"; counter +=1; append posQuote pos) else exit
				)
				if secondChar != "" do
				(
					for i = 1 to theExpression.count do
					(
						pos = findstring theExpression secondChar
						if pos != undefined then (theExpression[pos] = "*"; counter +=1; append posQuote pos) else exit
					)
				)
				if (mod counter 2) == 0 then true else false
			),
			fn compileIndexSort =
			(
				IndexSortClassName = "PathScripts.IndexSort"
				if classof (dotnet.GetType IndexSortClassName) != dotNetObject do
				(
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

							public static string[] reOrderString(string[] theArray, int[] theKeyIndexArray)
							{
								return reOrder<string>(theArray, theKeyIndexArray);
							}
							public static int[] reOrderInt(int[] theArray, int[] theKeyIndexArray)
							{
								return reOrder<int>(theArray, theKeyIndexArray);
							}
							public static Single[] reOrderFloat(Single[] theArray, int[] theKeyIndexArray)
							{
								return reOrder<Single>(theArray, theKeyIndexArray);
							}

							public static T[] reOrder<T>(T[] theArray, int[] theKeyIndexArray)
							{
								Array.Sort(theKeyIndexArray, theArray);
								return theArray;
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
					
					::P3D_FileAssemblyIndexSort = compilerResults.CompiledAssembly -- Ésta tiene que ser la global anterior
				)
			),
			
			public
			fn Equals x y result:on = if not result then result else	--	Function by DenisT. Equality of arrays
			(
				if (iskindof x Array) and (iskindof y Array) and x.count == y.count then
				(
					for k=1 to x.count while result do result = Equals x[k] y[k] result:result
					result
				)
				else (x == y)
			),		
			---------------------------------------------
			--	HELP Function
			---------------------------------------------
			fn Help =	--	General Help for maxLINQ. List of Functions.
			(
				helpmsg = ("
	QUERY FUNCTIONS:(Use a Boolean Lambda Expression. Don't affect to array elements values. Allow further queries)
		_Where: Selects the array elements that satisfy a Boolean Lambda Expression.
		W: same than _Where.
		mappedProperty: Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the Boolean Lambda Expression.
		mappedProp: same than mappedProperty.	
		mappedGeometry: Collects the 'Geometry' of the Geometry_Objects in the array that satisfy the Boolean Lambda Expression.
		mappedGeom: same than mappedGeometry.	
		DotNetIterator: Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the Boolean Lambda Expression.
		DotNetIteratorGet: Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
		TakeWhile: Returns the array elements as long as the Boolean Lambda Expression is true
		SkipWhile: Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
					
	FILTER FUNCTIONS:(Most use a Lambda Expression for projecting array values. Array values change for 'Select'=='Apply' functions. Allow further queries)
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
		ToArray: Same as Show
		Count: Returns the number of elements in the result array
		Sum: Returns the addition of the array elements
		Average: Returns the average value of the array elements
		_min: Returns the minimum value of the array elements
		_max: Returns the maximum value of the array elements
		Range: Generates a sequence of numbers within a specified range
		Repeat: Generates a sequence that contains one repeated value a specified number of times.
		All: Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
		Any: Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
		First: Selects the first array element that satisfy the Boolean Lambda Expression.
		Last: Selects the last array element that satisfy the Boolean Lambda Expression.
		Contains: Determines whether an array contains a specified value. 
		SequenceEqual: Determines whether two sequences are equal by comparing the elements.
		Compare: Same as SequenceEqual
		isEmpty: Determines if the array is empty.
		updateObjects: updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
		------------------------------------------------------------------------------------------------------
	MANAGER FUNCTIONS:
		maxLinqMan.help(): shows this message (summary of functions)
		maxLinqMan.helpFN \"<function name>\": for help on each maxLinq function
		maxLinqMan.helpFN \"LA\": for help on LambdExpressions syntax
		maxLinqMan.helpMAN(): for help on MaxLinq Manager Functions
		maxLinqMan.check \"<Query Function Expression>\": checks for validity of a query expression (see maxLinqMan.helpMAN())
		maxLinqMan.isInstance <qarray>: checks if <qarray> is a maxLinq instance
		maxLinqMan.dispose <[&]qarray>: voids <qarray> maxLinq instance
		maxLinqMan.createInstance <array value>: creates a maxLinq instance
		maxLinqMan.ToArray <qarray>: returns the array value of a maxLinq instance
	")		
				print helpmsg
				ok
			),
			
			fn helpFN fnName =
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
				-- Syntax: separated by comas (',') if more than one and ended by colon (':'). Can be uppercase.
				-- q: indicates that it's not the last function in the query. Thus, the returned value will be a struct, not an array.
				-- s: works in subarray elements if the array is a multiarray (valid in most query functions)
				-- i: returns indices of elements instead of the elements value (valid in most query functions)
				-- index: allows to use the variable 'index' in the lambda expression which holds the index of the element (valid in some query functions)
				-- obj: only valid in the 'mappedProperty', 'mappedGeometry', '_Where' and 'Select' functions. Allows to use the variable 'obj' in the lambda expression which holds the maxObject element in the array or of a previous query in '_Where' and 'Select' cases.
				-- key: only valid in the 'mappedProperty', 'mappedGeometry' and 'GroupBy' functions. The result query will be an array of structs with 3 parameters: key, value and count (see each case in their respective help).
				-- d: only valid in the 'Sort' query function. The sort will be done in descending order
				-- ci: The comparisons will be caseInsensitive for strings. NOTE: Functions are CaseSensitive by default.
		------------------------------------------------------------------------------------------------------
	")
	--	QUERY FUNCTIONS
					"_where": helpmsg =(		--	101
	"
	 _where LA : Query Function
					--	<ArrayQuery>._Where(<BooleanLambdaExpression var>): Selects the array elements that satisfy the Boolean Lambda Expression.
					--	<ArrayQuery>._Where#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
					--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query. 
					--	Accepted modifiers: q, s, i, obj, index
					--
					-- 'String maxLINQ':
					-- <StringQuery>._Where(<stringToSearch>): Returns an array with the indices of all the matches of the 'stringToSearch' in the original string
					-- <StringQuery>._Where#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
					-- Accepted modifiers: ci
						
					/* -- Examples
					myVar = 10 -- extVar 
					qarray = maxLinq #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'x => x > 15')								--> #(16, 17, 18, 19, 20)
					qarray = maxLinq #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'i: x => x > 15')							--> #(7, 8, 9, 10, 11)
					qarray = maxLinq #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where(#'i,index: x => x > 15 and index < 10')		--> #(7, 8, 9)
					qarray = maxLinq #(10,11,12,13,14,15,16,17,18,19,20); qarray._Where#(myVar, #'x myVar => x+myVar > 25')			--> #(16, 17, 18, 19, 20)
					qarray = maxLinq #(10,11,12,13,14,15,16,17,18,19,20); (qarray._Where(#'q: x => x > 15')).sum#()					--> 90
					
					qarray = maxLinq #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where(#'s: subitem => subitem > 18')				--> #(#(19, 20), #(25, 38, 19, 20))
					qarray = maxLinq #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where(#'s,i: subitem => subitem > 18')				--> #(#(10, 11), #(1, 2, 8, 9))
					qarray = maxLinq #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); qarray._Where#(myVar, #'s:x myVar => x+myVar > 25')			--> #(#(16, 17, 18, 19, 20), #(25, 38, 16, 17, 18, 19, 20))
					qarray = maxLinq #(#(10,11,12,13,14,15,16,17,18,19,20),#(25,38,7,15,16,17,18,19,20)); (qarray._Where(#'q,s: subitem => subitem > 15')).sum(#'s:')	--> #(90, 153)
					*/
					
					/* -- Examples for 'String maxLINQ'
					qarray = _From \"Hello World\"; qarray._Where(\"o\")			--> #(5, 8)
					qarray = _From \"Hello World\"; qarray._Where(\"O\")		--> #()
					qarray = _From \"Hello World\"; qarray._Where#(\"O\",\"ci:\")		--> #(5, 8)
					qarray = _From \"Hello World Hello World\"; qarray._Where(\"llo\")	--> #(3, 15)
					qarray = _From \"Hello World Hello World\"; qarray._Where(\"lla\")	--> #()
					*/
	")
					"w": helpmsg =(		--	102
	"
	W LA: Query Function (see '_Where' help)
	")
					"mappedproperty": helpmsg =(		--	103
	"
	mappedProperty LA : Query Function
					--	<ArrayQuery>.mappedProperty(<BooleanLambdaExpression property var>): Collects the 'mapped property' of the MaxObject_elements in the array that satisfy the  Boolean Lambda Expression.
					--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the original maxObject_element
					--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
												--	Key: holds the MaxObject
												--	Value: holds the array with the result properties of the query for the key MaxObject
												--	Count: the number of elements in the array of properties for this MAxObject
					--	Some mapped Properties: verts, faces, edges, selectedVerts, selectedFaces, selectedEdges, children, modifiers, controller.Keys...
					--	Accepted modifiers: q, s, i, obj
					--	Allways returns a multisubarray with each subArray corresponding to the catched properties of each MaxObject in the original array
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
						
					-- Ex.: 	qarray.mappedProperty(#'modifiers mod => isKindOf mod Blend')
									qarray.mappedProperty(#'i, obj: Keys k => k.Time > 100f and obj.pos.z >= 0')
									(qarray.mappedProperty(#'SelectedVerts v => v.pos.z >= 0')).Apply(#'v => move v [-100,0,0]')
						
	")
					"mappedprop": helpmsg =(		--	104
	"
	mappedProp LA: Query Function (see 'mappedProperty' help)
	")
					"mappedgeometry": helpmsg =(		--	105
	"
	mappedGeometry LA : Query Function
					--	<ArrayQuery>.mappedGeometry(<BooleanLambdaExpression GeometyProperty var>): Collects the  'Geometry' of the Geometry_Objects in the array that satisfy the  Boolean Lambda Expression.
					--	Only valid for Verts, Faces and Edges and their 'Selected' ones, for Editable_Meshes and Editable_Poly
					--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the original maxObject_element
					--	Special modifier 'key': the result is a new array of 'lookUp struct' with 3 properties:
												--	Key: holds the geometry object
												--	Value: holds the array with the result of the query for the key object
												--	Count: the number of elements in the array
					--	Accepted modifiers: q, s, key, i
					--	Allways returns a multisubarray with each subArray corresponding to the catched geometry of each MaxObject in the original array
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
						
					-- Ex.: 	qarray.mappedGeometry(#'Verts v => v.z > 0')
									(qarray.mappedGeometry(#'i,q: SelectedVerts v => v.z <= 0')).Select(#'obj:v => getNormal obj v')
									(qarray.mappedGeometry(#'i,key,q: SelectedVerts v => v.z <= 0')).Select(#'st=>st.key.selectedVerts = st.value')
	")
					"mappedgeom": helpmsg =(		--	106
	"
	mappedGeom LA: Query Function (see 'mappedGeometry' help)
	")
					"dotnetiterator": helpmsg =(		--	107
	"
	DotNetIterator LA : Query Function
					--	<ArrayQuery>.DotNetIterator(<BooleanLambdaExpression CollectionItem>): Collects the Items of a 'Dotnet Iteratable Collection' that satisfy the  Boolean Lambda Expression.
					--	Not Reserved words in lambda expression
					--	Some Dotnet Iteratable: List, \"System.Management.ManagementClass\" instances ...
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
						
					-- Ex.: 	QArray.DotNetIterator(#'x=>true')
									(QArray.DotNetIterator(#'q: x=>x.name == \"ProcessorId\"')).Apply(#'x=>x.value')
	")
					"dotnetiteratorget": helpmsg =(		--	108
	"
	DotNetIteratorGet LA : Query Function
					--	<ArrayQuery>.DotNetIteratorGet(<BooleanLambdaExpression CollectionItem>): Collects a value/property from the Items of a 'Dotnet Iteratable Collection' applying the Lambda Expression to the Item.
					--	Not Reserved words in lambda expression
					--	Some Dotnet Iteratable: List, \"System.Management.ManagementClass\" instances ...
					--	Accepted modifiers: q, s
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
						
					-- Ex.: 	QArray.DotNetIteratorGet(#'x=>x.Properties')
									(QArray.DotNetIteratorGet(#'q:x=>x.Properties')).DotNetIterator(#'s:x=>x.name == \"ProcessorId\"')
	")
					"takewhile": helpmsg =(		--	109
	"
	TakeWhile LA : Query Function
						--	<ArrayQuery>.TakeWhile(<BooleanLambdaExpression var>): Returns the array elements as long as the Boolean Lambda Expression is true
						--	<ArrayQuery>.TakeWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
						--	Accepted modifiers: q, s, i, index
						--
						-- 'String maxLINQ':
						-- <StringQuery>.TakeWhile(<stringToSearch>): Returns the string before the first occurrence of 'stringToSearch'
						-- <StringQuery>.TakeWhile#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
	")
					"skipwhile": helpmsg =(		--	110
	"
	SkipWhile LA : Query Function
						--	<ArrayQuery>.SkipWhile(<BooleanLambdaExpression var>): Bypasses elements in the array as long as the Boolean Lambda Expression is true and then returns the remaining elements
						--	<ArrayQuery>.SkipWhile#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
						--	Accepted modifiers: q, s, i, index
						--
						-- 'String maxLINQ':
						-- <StringQuery>.SkipWhile(<stringToSearch>): Returns the string after the first occurrence of 'stringToSearch' ('stringToSearch' included)
						-- <StringQuery>.SkipWhile#(<stringToSearch>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
	")

	--	FILTER FUNCTIONS
					"Select": helpmsg =(		--	201
	"
	Select LA : Filter Function
				--	<ArrayQuery>.Select(<LambdaExpression var>): Creates a new array from input array by applying Lambda Expression to array elements (the result array has the same number of elements). Changes array values. 
				--	<ArrayQuery>.Select#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Reserved word 'obj' in lambda expression in case of 'obj:' modifier, which holds the MaxObject of a previous mappedProperty or mappedGeometry query.
				--	Accepted modifiers: q, s, obj, index
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String MaxLINQ'. Use 'Replace' or 'ReplaceAt' functions to modify the string
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
					-- This Function is not valid for 'String MaxLINQ'. 
	")
					"makeunique": helpmsg =(		--	204
	"
	MakeUnique LA : Filter Function
					--	<ArrayQuery>.MakeUnique#(): Creates a new array with unique elements. Uses 'Default Equality' function for comparing arrays
					--	<ArrayQuery>.MakeUnique(<LambdaExpression>): compares array elements after applying the Lambda expression to each element of the array
					--	Reserved Lambda expressions (\"s:\"), (\"i:\"), (\"i,s:\") and (\"s,i:\") - with optional 'q' modifier - for fast calculation in case of subArray or Index modifiers without a Lambda Expression Value Comparison
					--	Accepted modifiers: q, s, i
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
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
					-- This Function is not valid for 'String MaxLINQ'. 
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
					-- This Function is not valid for 'String MaxLINQ'. 
						
					/* -- Examples
						
					*/
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
					-- This Function is not valid for 'String MaxLINQ'. 
	")
					"flatten": helpmsg =(		--	209
	"
	flatten : Filter Function
				--	<ArrayQuery>.Flatten#(): Flattens one level of the array
				--	<ArrayQuery>.Flatten(\"q:\"): same than above but not final result
				--	Accepted modifiers: q
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String MaxLINQ'. 
	")
					"flattenall": helpmsg =(		--	210
	"
	flattenAll : Filter Function
					--	<ArrayQuery>.flattenAll#(): Flattens all levels of the array. The result array is a one dimension array.
					--	<ArrayQuery>.flattenAll(\"q:\"): same than above but not final result
					--	Accepted modifiers: q
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. 
	")
					"sort": helpmsg =(		--	211
	"
	Sort LA : Filter Function
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
					-- This Function is not valid for 'String MaxLINQ'. 
	")
					"orderby": helpmsg =(		--	212
	"
	orderBy LA : Filter Function (see 'Sort' help)
	")
					"reorder": helpmsg =(		--	213
	"
	ReOrder indexArray : Filter Function
						--	<ArrayQuery>.ReOrder(indexArray): Returns the array reordered based on the arrayIndex order. All the elements in array must be comparable.
						--	<ArrayQuery>.ReOrder#(indexArray, \"s:\"): Returns a multiarray with the reordered subarrays based on the arrayIndex subarrays order. All the elements in subArrays must be comparable
						--	indexArray must be an integer array (or multiarray) of the same length than the array to sort.
						--	Accepted modifiers: q, s
						--
						-- 'String maxLINQ':
						-- This Function is not valid for 'String MaxLINQ'. 
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
						
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.reverse#()			--> \"dlroW olleH\"
						*/
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
						
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.Take(5)			--> \"Hello\"
						qarray = _From \"Hello World\"; (qarray.Take#(5,\"q:\")).Contains(\"W\")		--> False
						*/
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
						
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.Skip(6)			--> \"World\"
						qarray = _From \"Hello World\"; (qarray.Skip#(6,\"q:\")).Contains(\"W\")		--> True
						*/
	")
					"save": helpmsg =(		--	217
	"
	Save &var : Filter Function
						-- <ArrayQuery>.Save(&var).<next query function>: Saves the temporary query result to an existing variable name in the actual scope
						-- Variable name 'var' must exist in the actual scope
						-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String MaxLINQ'. 
	")
					"debug": helpmsg =(		--	218
	"
	Debug stringMessage : Filter Function
						-- <ArrayQuery>.Debug#().<next query function>: Prints the temporary query result to Listener
						-- <ArrayQuery>.Debug(string_message).<next query function>: Prints the temporary query result to Listener preceded by the input string message
						-- This function doesn't accept any modifier ('q:' modifier is implicitly allways applied)
						--
						-- 'String maxLINQ':
						-- This Function is valid for 'String MaxLINQ'. 
	")
					"concat": helpmsg =(		--	219
	"
	Concat otherArray : Filter Function
						--	<ArrayQuery>.Concat(otherArray): Creates a new array concatenating all elements of original array and 'otherArray'. 
						--	<ArrayQuery>.Concat#(otherArray, <LambdaExpression var>): concatenates both arrays after applying the Lambda expression to each element of 'otherArray'.
						--	Reserved Lambda expressions (\"s:\") - with optional 'q' modifier - for fast calculation in case of subArray modifier without a Lambda Expression Value
						--	This function doesn't \"MakeUnique\" result.
						--	Accepted modifiers: q, s
						--
						-- 'String maxLINQ':
						-- <StringQuery>.Concat(<stringToAppend>): Appends 'stringToAppend' to the original string
						-- Accepted modifiers: q
						
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.concat(\" and City\")			--> \"Hello World and City\"
						*/
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
						
						/* -- Examples:
						qarray = _From #(1,2,3,5,6,7,8,5,9,10); qarray.replace#(5,555)				--> #(1, 2, 3, 555, 6, 7, 8, 555, 9, 10)
						qarray = _From #(1,2,3,5,6,7,8,5,9,10); qarray.replace#(0,555,#'x=>mod x 2')		--> #(1, 555, 3, 5, 555, 7, 555, 5, 9, 555)
						qarray = _From #(\"AAA\",\"bbb\",\"aaa\",\"BBB\"); qarray.replace#(\"aaa\",\"CCC\")	--> #(\"AAA\", \"bbb\", \"CCC\", \"BBB\")
						qarray = _From #(\"AAA\",\"bbb\",\"aaa\",\"BBB\"); qarray.replace#(\"aaa\",\"CCC\",\"ci:\")	--> #(\"CCC\", \"bbb\", \"CCC\", \"BBB\")
						qarray = _From #(#(1,2,3,4,5),#(5,4,3,2,1)); qarray.replace#(5,555,#'s:')		--> #(#(1, 2, 3, 4, 555), #(555, 4, 3, 2, 1))
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.replace#(\"world\",\"City\")			--> \"Hello World\"
						qarray = _From \"Hello World\"; qarray.replace#(\"world\",\"City\",\"ci:\")		--> \"Hello City\"
						qarray = _From \"Hello World\"; qarray.replace#(\"o\",\"OOO\",\"ci:\")			--> \"HellOOO WOOOrld\"
						qarray = _From #(\"Hello\",\"World\"); qarray.select(#'x=>(_From x).replace#(\"o\",\"_A_\")')	--> #(\"Hell_A_\", \"W_A_rld\")
						*/
	")
					"replaceat": helpmsg =(		--	221
	"
	ReplaceAt TriArray : Filter Function
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex): Replaces the occurrence of 'someValue' at 'atIndex' position with 'otherValue' if the match is true
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, \"s:\"): Same as above but searchs 'someValue' in the subarray elements. 
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, \"ci:\"): forces a case Insensitive comparison for strings
						--	<ArrayQuery>.ReplaceAt#(someValue, otherValue, atIndex, <LambdaExpression var>):  Determines the equality to 'someValue' of the array element at 'atIndex' position after apllying it the Lambda Expression (i.e., item must evaluate to 'someValue').
						--	Accepted modifiers: s, ci, q
						--	'someValue' and 'otherValue' can be an array. In this case, default array equality is used (allways case sensitive)
						--
						-- 'String maxLINQ':
						-- <StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex): Replaces the occurrence of 'stringToReplace' at 'atIndex' position with 'ReplacingString' if the match is true
						-- <StringQuery>.ReplaceAt#(<stringToReplace>, <ReplacingString>, atIndex, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci, q
						
						/* -- Examples:
						qarray = _From #(1,2,3,4,5,6,7,8,9)); qarray.ReplaceAt#(5,555,6)				--> #(1, 2, 3, 4, 5, 6, 7, 8, 9)
						qarray = _From #(1,2,3,4,5,6,7,8,9)); qarray.ReplaceAt#(5,555,5)				--> #(1, 2, 3, 4, 555, 6, 7, 8, 9)
						qarray = _From #(1,2,3,4,5,6,7,8,9)); qarray.ReplaceAt#(0,555,4,#'x=>mod x 2')	--> #(1, 2, 3, 555, 5, 6, 7, 8, 9)
						qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.ReplaceAt#(\"bbb\",\"ddd\",2)	--> #(\"AAA\", \"BBB\", \"CCC\")
						qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.ReplaceAt#(\"bbb\",\"ddd\",2,\"ci:\")	--> #(\"AAA\", \"ddd\", \"CCC\")
						
						qarray = _From #(#(1,2,3,4,5,6,7,8,9),#(1,2,3,4,5,6,7,8,9));
						qarray.ReplaceAt#(5,555,5,\"s:\")			--> #(#(1, 2, 3, 4, 555, 6, 7, 8, 9), #(1, 2, 3, 4, 555, 6, 7, 8, 9))
						qarray.ReplaceAt#(0,555,4,#'s:x=>mod x 2')		--> #(#(1, 2, 3, 555, 5, 6, 7, 8, 9), #(1, 2, 3, 555, 5, 6, 7, 8, 9))
						qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); 
						qarray.ReplaceAt#(\"bbb\",\"ddd\",2,\"s,ci:\")	--> #(#(\"AAA\", \"ddd\", \"CCC\"),#(\"AAA\", \"ddd\", \"CCC\"))
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"o\",\"OO\",6)			--> \"Hello World\"
						qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"o\",\"OO\",5)			--> \"HellOO World\"
						qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"O\",\"OO\",5)		--> \"Hello World\"
						qarray = _From \"Hello World\"; qarray.ReplaceAt#(\"O\",\"OO\",5,\"ci:\")		--> \"HellOO World\"
						*/
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
						
						/* -- Examples:
						qarray = _From #(1,2,3,1,2,3,1,2); qarray.remove(3)				--> #(1, 2, 1, 2, 1, 2)
						qarray = _From #(1,2,3,1,2,3,1,2); qarray.remove#(1,#'x=>mod x 2')		--> #(2, 2, 2)
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,2)		--> #(#(3, 1), #(2, 3))
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,#'s:')	--> #(#(2), #(3), #(2, 3), #(2))
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.remove#(1,#'s:x=>mod x 2')	--> #(#(2), #(), #(2), #(2))
						qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.remove(\"bbb\")	--> #(\"AAA\", \"BBB\", \"CCC\")
						qarray = _From #(\"AAA\",\"BBB\",\"CCC\"); qarray.remove(\"bbb\",\"ci:\")	--> #(\"AAA\", \"CCC\")
						qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); qarray.remove#(true,#'ci,s:x=> x<\"C\"')	--> #(#(\"CCC\"), #(\"CCC\"))
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.remove#(\"L\",\"ci:\")			--> \"Heo Word\"
						qarray = _From \"Hello World Hello World\"; qarray.remove#(\"hello \",\"ci:\")		--> \"World World\"
						*/
	")
					"removeat": helpmsg =(		--	222
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
						
						/* -- Examples:
						qarray = _From #(1,2,3,1,2,3,1,2); removeAt#(2,4)				--> #(1, 2, 3, 1, 2, 3, 1, 2)
						qarray = _From #(1,2,3,1,2,3,1,2); removeAt#(1,4)				--> #(1, 2, 3, 2, 3, 1, 2)
						qarray = _From #(1,2,3,1,2,3,1,2); qarray.removeAt#(1,4,#'x=>mod x 2')	--> #(1, 2, 3, 2, 3, 1, 2)
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(#(1,2),4)		--> #(#(1, 2), #(3, 1), #(2, 3))
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(#(2,4),4,#'x=>#(x[1]*2,x[2]*2)')	--> #(#(1, 2), #(3, 1), #(2, 3))
						qarray = _From #(#(1,2),#(3,1),#(2,3),#(1,2)); qarray.removeAt#(1,1,\"s:\")	--> ##(#(2), #(3, 1), #(2, 3), #(2))
						qarray = _From #(#(\"AAA\",\"BBB\",\"CCC\"),#(\"AAA\",\"BBB\",\"CCC\")); qarray.removeAt#(\"bbb\",2,\"s,ci:\")	--> #(#(\"AAA\", \"CCC\"), #(\"AAA\", \"CCC\"))
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.removeAt#(\"o\",6)			--> \"Hello World\"
						qarray = _From \"Hello World\"; qarray.removeAt#(\"o\",5)			--> \"Hell World\"
						qarray = _From \"Hello World\"; qarray.remove#(\"O\",5,\"ci:\")	--> \"Hell World\"
						*/
	")
	
	
	

	--	FINAL FUNCTIONS
					"show": helpmsg =(		--	301
	"
	Show : Final Function
			--	(<ArrayQuery>[.<sequence of query or filter functions>]).Show(): Returns the result array after previous Query and Filter functions
			-- <ArrayQuery>.Show(): retrieves the last query as array.
			--	Only valid after a function query with 'q:' modifier or as first function to retrieve the last query.
			--
			-- 'String maxLINQ':
			-- This Function is valid for 'String MaxLINQ'. 
	")
					"toarray": helpmsg =(		--	302
	"
	 ToArray : Final Function (see 'Show' help)
	")
					"count": helpmsg =(		--	303
	"
	Count LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count#(): Returns the number of elements in the result array
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count(\"s:\"): Returns an array with the number of elements of each item in the result array
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count(<BooleanLambdaExpression var>): Returns the number of elements in the result array that satisfy the Boolean Lambda Expression.
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Count#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Accepted modifiers: s, index
				--
				-- 'String maxLINQ':
				-- <StringQuery>[. <sequence of query or filter functions>]).Count#(): Returns the length of the string
				-- Accepted modifiers: none
	")
					"sum": helpmsg =(		--	304
	"
	Sum LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum#(): Returns the standard addition of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum(\"s:\"): Returns an array with the standard addition of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum(<LambdaExpression var>):  Returns the addition of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Sum#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Accepted modifiers: s, index
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String MaxLINQ'. 
	")
					"average": helpmsg =(		--	305
	"
	Average LA : Final Function
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average#(): Returns the standard average value of the array elements
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average(\"s:\"): Returns an array with the standard average of each subarray element
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average(<LambdaExpression var>):  Returns the Average of the array elements after applying the Lambda Expression to each element. Changes array Values
				--	(<ArrayQuery>[.<sequence of query or filter functions>]).Average#(<externalVar>, <LambdaExpression var extVar>): same as above using an external variable/maxObject
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Accepted modifiers: s, index
				--
				-- 'String maxLINQ':
				-- This Function is not valid for 'String MaxLINQ'. 
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
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Accepted modifiers: s, i, ci, index
				--
				-- 'String maxLINQ':
				-- (<StringQuery>[. <sequence of query or filter functions>])._min#(): Returns the minimum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"i:\"): Returns index of the minimum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"ci:\"): same as above using case insensitive comparing
				-- (<StringQuery>[. <sequence of query or filter functions>])._min(\"ci, i:\"): same as above using case insensitive comparing and returning the index
				-- Accepted modifiers:  i, ci
						
				/* -- Examples:
				
				*/
				
				/* -- Examples for 'String maxLINQ'
				qarray = _From \"helloWorld\"; qarray._min#()			--> \"W\"
				qarray = _From \"helloWorld\"; qarray._min(\"i:\")			--> 6
				qarray = _From \"helloWorld\"; qarray._min(\"ci:\")			--> \"d\"
				qarray = _From \"helloWorld\"; qarray._min(\"i, ci:\")			--> 10
				*/
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
				--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
				--	Accepted modifiers: s, i, ci, index
				--
				-- 'String maxLINQ':
				-- (<StringQuery>[. <sequence of query or filter functions>])._max#(): Returns the maximum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"i:\"): Returns index of the maximum character of the string
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"ci:\"): same as above using case insensitive comparing
				-- (<StringQuery>[. <sequence of query or filter functions>])._max(\"ci, i:\"): same as above using case insensitive comparing and returning the index
				-- Accepted modifiers:  i, ci
						
				/* -- Examples:
				
				*/
				
				/* -- Examples for 'String maxLINQ'
				qarray = _From \"helloWorld\"; qarray._max#()			--> \"r\"
				qarray = _From \"helloWorld\"; qarray._max(\"i:\")			--> 8
				qarray = _From \"helloWorld\"; qarray._max(\"ci:\")			--> \"W\"
				qarray = _From \"helloWorld\"; qarray._max(\"i, ci:\")			--> 6
				*/
	")
					"range": helpmsg =(		--	308
	"
	Range valuePair : Final Function
					--	<ArrayQuery>.Range#(int1, int2): Generates a sequence of integer numbers within a specified range (incremented by 1).
					--	<ArrayQuery>.Range([int1, int2]): Generates a sequence of float numbers within a specified range (incremented by 1.0).
					--	<ArrayQuery>.Range#(int1, int2, step): Generates a sequence of integer numbers within a specified range incremented by 'step' integer value.
					--	<ArrayQuery>.Range([int1, int2, step]): Generates a sequence of float numbers within a specified range incremented by 'step' float value.
					--	The original array is not affected
					--	Accepted modifiers: none
					--
					-- 'String maxLINQ':
					-- <StringQuery>.Range#(int1, int2): Returns a string with the sequence of characters from ASCII int1 to ASCII int2
					-- Accepted modifiers: none
						
				/* -- Examples:
				
				*/
				
				/* -- Examples for 'String maxLINQ'
				(_from \"\").range#(65,90)			--> \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"
				*/
	")
					"repeat": helpmsg =(		--	309
	"
	Repeat valuePair : Final Function
					--	<ArrayQuery>.Repeat#(value, times): Generates a sequence that contains one repeated value a specified number of times.
					--	The original array is not affected
					--	Accepted modifiers: none
					--
					-- 'String maxLINQ':
					-- <StringQuery>.Repeat#(stringValue, times): Returns a string with the 'stringValue' repeated a specified number of times.
					-- Accepted modifiers: none
						
				/* -- Examples:
				
				*/
				
				/* -- Examples for 'String maxLINQ'
				(_from \"\").repeat#(\"Aa\",10)			--> \"AaAaAaAaAaAaAaAaAaAa\"
				*/
	")
					"all": helpmsg =(		--	310
	"
	All LA : Final Function
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).All(<BooleanLambdaExpression var>): Returns TRUE if all the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).All#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
					--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
					--	Accepted modifiers: s, index
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. Use 'Compare' function to query equality of strings
	")
					"any": helpmsg =(		--	311
	"
	Any LA : Final Function
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).Any(<BooleanLambdaExpression var>): Returns TRUE if at least one of the array elements satisfy the Boolean Lambda Expression, FALSE otherwise
					--	(<ArrayQuery>[.<sequence of query or filter functions>]).Any#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
					--	In case of subarray modifier 's:', returns an array of boolean values corresponding to each subarray element satisfying or not the condition.
					--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
					--	Accepted modifiers: s, index
					--
					-- 'String maxLINQ':
					-- This Function is not valid for 'String MaxLINQ'. Use 'Contains' function to query if a sub_string is included in the string
	")
					"first": helpmsg =(		--	312
	"
	First LA : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).First(<BooleanLambdaExpression var>): Selects the first array element that satisfy the Boolean Lambda Expression.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).First#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Returns 'undefined' if any element satisfy the Boolean condition
						--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
						--	Accepted modifiers: s, i, index
						--
						-- 'String maxLINQ':
						-- This Function has the same behavior than 'Contains' function whith the 'i:' modifier
						-- (<StringQuery>[.<sequence of query or filter functions>]).First(<stringToFind>): Returns the index of the first occurrence of 'stringToFind' (0 if none)
						-- (<StringQuery>[.<sequence of query or filter functions>]).First#(<stringToFind>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
								
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.First(\"o\")		--> 5
						qarray = _From \"Hello World\"; qarray.First(\"O\")		--> 0
						qarray = _From \"Hello World\"; qarray.First#(\"O\",\"ci:\")	--> 5
						*/
	")
					"last": helpmsg =(		--	313
	"
	Last LA : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Last(<BooleanLambdaExpression var>): Selects the last array element that satisfy the Boolean Lambda Expression.
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).Last#(<externalVar>, <BooleanLambdaExpression var extVar>): same as above using an external variable/maxObject
						--	Returns 'undefined' if any element satisfy the Boolean condition
						--	Reserved word 'index' in lambda expression in case of 'index:' modifier, which holds the index of each element
						--	Accepted modifiers: s, i, index
						--
						-- 'String maxLINQ':
						-- (<StringQuery>[.<sequence of query or filter functions>]).Last(<stringToFind>): Returns the index of the Last occurrence of 'stringToFind' (0 if none)
						-- (<StringQuery>[.<sequence of query or filter functions>]).Last#(<stringToFind>, \"ci:\"): same as above using case insensitive comparison
						-- Accepted modifiers: ci
								
						/* -- Examples:
						
						*/
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.Last(\"o\")		--> 8
						qarray = _From \"Hello World\"; qarray.Last(\"O\")		--> 0
						qarray = _From \"Hello World\"; qarray.Last#(\"O\",\"ci:\")	--> 8
						*/
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
						
						/* -- Examples for 'String maxLINQ'
						qarray = _From \"Hello World\"; qarray.Contains(\"w\")		--> False
						qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"ci:\")		--> True
						qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"i,ci:\")		--> 7
						qarray = _From \"Hello World\"; qarray.Contains#(\"w\",\"i:\")		--> 0
						*/
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
						
						/* -- Examples
						qarray = maxLinq #(#(10, 20), #(30, 40))
						
						qarray.sequenceEqual#(#(10,20),#(30,40))	--> True
						qarray.sequenceEqual#(#(10,20),#(30,41))	--> False
						qarray.sequenceEqual#(#(#(11,21),#(31,41)),#'x=>#(x[1]/10,x[2]/10)')	-->	True
						qarray.sequenceEqual#(#(#(11,20),#(30,40)),#'s:')	-->	#(false, true)
						*/
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

						/* -- Examples
						qarray = maxLinq #(); qarray.isempty#()							--> True
						qarray = maxLinq #(1); qarray.isempty#()						--> False
						qarray = maxLinq #(#(),#()); qarray.isempty#()					--> False
						qarray = maxLinq #(#(),#()); qarray.isempty(#'s:')				--> #(true, true)
						qarray = maxLinq #(#(),#()); (qarray.flatten(#'q:')).isempty#()	--> True
						qarray = maxLinq #($); if ((qarray.mappedProp(#'q:modifiers mod=>isKindOf mod Bend')).flatten(#'q:')).isEmpty#() do (addModifier $ (Bend()))	--> add Bend modifier if not present
						qarray = maxLinq #($); (qarray.mappedProp(#'q:modifiers mod=>isKindOf mod Bend')).Apply(#'obj: x=>addModifier obj (Bend())') --> same as above more efficient.
						*/
	")
					"compare": helpmsg =(		--	317
	"
	 Compare otherArray : Final Function (see 'SequenceEqual' help)
	")
					"updateobjects": helpmsg =(		--	318
	"
	updateObjects : Final Function
						--	(<ArrayQuery>[.<sequence of query or filter functions>]).updateObjects(): updates GeometryClass objects in case of previous queries using mappedProp or mappedGeom on GeometryClass objects
						--	Accepted modifiers: none
						--	Returns previous array query result.
						--
						-- 'String maxLINQ':
						-- This Function is not valid for 'String MaxLINQ'. 

						/* -- Examples
						qarray = _From geometry;
						(((qarray._Where(#'q:node => isKindOf node Editable_Mesh')		--> select Editable_Mesh nodes...
									).mappedProp(#'q:verts vset => true')				--> collect all their vertices...
									).Apply(#'q,s:v=>v.pos += [0,0,10]')				--> move each vertex 10 units up in the Z axis...
									).UpdateObjects()	--> move all objects			--> update their geometry.
						*/
	")
	

				)
				print helpmsg
				ok
			),
			
			fn check LA =		--	Avoid esplicit arrays declarations (as #(1,2,3,4)) and replace dots between succesive Functions by vertical slash '|' . NOT VALID for String Queries
			(
				/*
				Example 1:
								maxLinqMan.check "mappedGeometry(\"i,key,q: SelectedVerts v => v.z <= 0\"))|Sum(\"s:\")"
				Example 2:
								maxLinqMan.check "mappedprop(#'q:verts v=>true')
												)|GroupBy(#'q,i,s,key:v=>(v.pos.z / 10) as integer')
												)|_where(#'q,s:vset=>abs (mod vset.key 2) == 1')
												)|Apply(#'obj,s: vset=>setVertSelection obj vset.value keep:true')"
				*/
				commands = #("_where","w","mappedproperty","mappedprop","mappedgeometry","mappedgeom","dotnetiterator","dotnetiteratorget","takewhile","skipwhile",	\
					"select","apply","groupby","makeunique","distinct","intersect","except","union","flatten","flattenall","sort","orderby","reorder","reverse","take","skip","save","debug","concat","replace","replaceat","remove",	\
					"show","toarray","count","sum","average","_min","_max","range","repeat","all","any","first","last","contains","sequenceequal","isempty","compare","updateobjects")
				commandsID = #(101,102,103,104,105,106,107,108,109,110,	\
					201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,	\
					301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318)
				acceptsVoidArray = #{204..205,209..212,214,218,303..307,316}
				acceptsArray = #{101..102,109..110,201..202,204..216,218..222,303..317}
				acceptsExternal = #{101..102,109..110,201..202,303..307,310..313}
				acceptsDirectValue = #{206..208,213,215..219,222,314..315,317}
				mustBeArray = #{220..221,309}
				voidArgument = #{301..302,318}
					
				theExpression = copy LA
				for i = 1 to LA.count do
				(
					pos = findstring theExpression "\n"
					if pos != undefined then (theExpression[pos] = "") else exit
				)
					
				arrayLA = filterString theExpression "|"
				if arrayLA.count == 1 do format "***\n*** Your Query Expression has only one Function. Remember to change '.' dot by '|' vertical slash between Functions for this check\n***"
				for k = 1 to arrayLA.count do
				(
					LA = arrayLA[k]
					format "\nQUERY EXPRESSION #%:\n" k 
					format "\t-- % --\n\n" LA
				
					firstPartIndex = findString LA "("
					if firstPartIndex == undefined do (print "\t**ERROR: Can`t find the open parenthesis for Query Expression"; continue)
					if firstPartIndex == 1 do (print "\t**ERROR: The Query Expression must start with a Query Function and not with an open parenthesis"; continue)
					secondPart = substring LA firstPartIndex -1
					if secondPart[secondPart.count] != ")" do (print "\t**ERROR: The Query Expression must end with a closing parenthesis"; continue)
					firstPart = substring LA 1 (firstPartIndex-1)
					isArray = firstPart[firstPart.count] == "#"
					if isArray then firstPart = trimleft (trimright firstPart " \t#") else trimleft (trimright firstPart)
					goodCommand = findItem commands (toLower firstPart)
					if goodCommand == 0 then (format "\t**ERROR: '%' is not a valid Query Function\n" firstPart; continue) else (format "\t** OK: '%' is a valid Query Function\n" firstPart)
					goodCommandID = commandsID[goodCommand]
					
					if not isArray and mustBeArray[goodCommandID] do (format "\t**ERROR: '%' Query Function only accepts array arguments\n" firstPart; continue)
					-- Case Range, Repeat, Replace
					if goodCommandID == 308 do format "\t** OK: Can't check '%' command. Argument must be an array of 2 or 3 integers, or a Point2 or a Point3" firstPart
					if goodCommandID == 309 do format "\t** OK: Can't check '%' command. Argument must be an array of 2 values, being the second one an integer" firstPart
					if goodCommandID == 220 do format "\t** OK: Can't check '%' command. Argument must be an array of 2 or 3 values" firstPart
					if goodCommandID == 221 do format "\t** OK: Can't check '%' command. Argument must be an array of 3 or 4 values" firstPart

					if isArray then
					(
						format "\t** OK: The Argument is an Array.\n"
						isOK = acceptsArray[goodCommandID]
						if isOK then (format "\t** OK: '%' Query Function accepts array arguments\n" firstPart) else (format "\t**ERROR: '%' Query Function doesn't accept array arguments\n" firstPart; continue)
						if (secondPart == "()") do
						(
							isOK = acceptsVoidArray[goodCommandID]
							if isOK then (format "\t** OK: '%' Query Function accepts void array argument #()\n" firstPart; continue) else (format "\t**ERROR: '%' Query Function doesn't accept void array argument #()\n" firstPart; continue)
						)
					)
					else
					(
						format "\t** OK: The Argument is NOT an Array.\n"
						if secondPart == "()" and not voidArgument[goodCommandID] do (format "\t**ERROR: '%' Query Function needs arguments\n" firstPart; continue)
						if secondPart != "()" and voidArgument[goodCommandID]  do (format "\t**ERROR: '%' Query Function doesn't accept arguments\n" firstPart; continue)
					)
						
					commaIndex =  findString secondPart ","; if commaIndex == undefined do commaIndex = 0
					sharpIndex =  findString secondPart "#"; if sharpIndex == undefined do sharpIndex = commaIndex
					singleQuoteIndex =  findString secondPart "'"; if singleQuoteIndex == undefined do singleQuoteIndex = commaIndex
					doubleQuoteIndex =  findString secondPart "\""; if doubleQuoteIndex == undefined do doubleQuoteIndex = commaIndex
					if isArray then
					(
						if commaIndex == 0 do (format "\t**ERROR:  It's an array but can't find the comma between the two items\n"; continue)
						if commaIndex > sharpIndex or commaIndex > singleQuoteIndex or commaIndex > doubleQuoteIndex do (format "\t**ERROR:  It's an array but can't find the comma between the two items\n"; continue)
						lambda = trimLeft (substring secondPart (commaIndex+1) -1)
					)
					else
					(
						if (commaIndex != 0) and (commaIndex < sharpIndex or commaIndex < singleQuoteIndex or commaIndex < doubleQuoteIndex) do (format "\t**ERROR:  It's NOT an array but have a comma before Lambda Expression\n"; continue)
						lambda = (trimLeft secondPart " \t(")
					)
					
					firstLambdaChar = substring lambda 1 1
					if firstLambdaChar != "#" and firstLambdaChar != "\"" do
					(
						if acceptsDirectValue[goodCommandID] then
						(
							format "\t** OK:Your Query Expression seems to be rigth. '%' Query Function accepts direct value. Check for parenthesis parity in case of problems persist\n" firstPart
						)
						else
						(
							format "\t**ERROR: '%' Query Functions needs a Lambda Expression that starts with a #' or a \" symbol \n" firstPart
						)
						continue
					)
					
					isName = firstLambdaChar == "#"
					if isName and (substring lambda 2 1 != "'") do (format "\t**ERROR: There's not a quote ' symbol after the # symbol for name expression\n"; continue)
					
					quote = if isName then "'" else "\""
					tempLambda = (trimRight lambda " \t)")
					if tempLambda[tempLambda.count] != quote do  (format "\t**ERROR: Your Lambda Expression doesn't have the end % quote symbol\n" quote; continue)
					
					isOK = parityCheck lambda "'"
					if not isOK do (format "\t**ERROR: There's not a parity for ' single quote symbol in your Lambda Expression\n"; continue)
					isOK = parityCheck lambda "\""
					if not isOK do (format "\t**ERROR: There's not a parity for \" double quote symbol in your Lambda Expression\n"; continue)
						
					LA = (trimRight lambda " \t)")
					format "\t** OK: Your Lambda Expression is: -- % -- \n" LA
					LA = trimLeft (trimRight LA " \t'\"") " \t#'\""
					
					initializeModifiers()
					soft = softParser LA
					if soft then
					(
						isOK = parityCheck secondPart "(" secondChar:")"
						if finalQuery and not isOK do (format "\t**ERROR: There's not a parity for '(' open and ')' closing parenthesis symbols in your Lambda Expression\n"; continue)
						if not finalQuery and isOK do (format "\t**ERROR: with the 'q:' modifier in your Lambda Expression, you must have one more ')' closing parenthesis symbol than '(' opening symbol\n"; continue)
					)
					else
					(
						isOK = findString  LA "=>"
						if isOK == undefined do (format "\t**ERROR: you don't have the assignment '=>' symbol in your Lambda Expression\n"; continue)
						try (parser LA) catch (format "\t**ERROR: something's wrong in your Lambda Expression. Check for ',' commas between modifiers and ':' after them\n"; continue)
						
						isOK = parityCheck secondPart "(" secondChar:")"
						if finalQuery and not isOK do (format "\t**ERROR: There's not a parity for '(' open and ')' closing parenthesis symbols in your Lambda Expression\n"; continue)
						if not finalQuery and isOK do (format "\t**ERROR: with the 'q:' modifier in your Lambda Expression, you must have one more ')' closing parenthesis symbol than '(' opening symbol\n"; continue)
						
						allvars = filterString vars " \t"
						if allvars.count == 1 and goodCommandID >= 103 and goodCommandID <= 106 do
						(
							format "\t**ERROR: '%' Query Function requires \"<property name>\" before the variable name before the '=>' symbol \n" firstPart; continue
						)
						if allvars.count != 1 and goodCommandID < 103 and goodCommandID > 106 do
						(
							isOK = acceptsExternal[goodCommandID]
							if not isOK do (format "\t**ERROR: '%' Query Function doesn't accept external variable in the Lambda Expression\n" firstPart; continue)
							if not isArray do (format "\t**ERROR: the Lambda Expression uses an external variable but the Query Expression for % function is not an array\n" firstPart; continue)
						)
						if goodCommandID >= 103 and goodCommandID <= 106 do
						(
							vars = (filterString vars " \t")[1]
							fnExpression = substring fnExpression (vars.count+2) -1
						)
						format "\t** OK: Your Lambda Function is: fn LA %\n" fnExpression
					)
					
					format "\t** OK: Your Query Expression #% seems to be rigth\n" k
				)
				print "\n***** END OF CHECKING *****"
				OK
			),
			
			fn isInstance qarray =	-- 	Returns TRUE if qarray is a maxLINQ instance
			(
				isKindOf qarray maxLINQ
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
			
			fn createInstance var =	--	returns a maxLINQ instance with 'var' value. 'var' must be an array or a mappable maxObject
			(
				maxLINQ var
			),
			
			fn ToArray qarray =	--	if qarray is a maxLINQ instance, returns its array value (Same as qarray.ToArray() method).
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
			
			fn helpMAN =
			(
				helpmsg = ("
	MANAGER FUNCTIONS:
		maxLinqMan.help(): shows the summary of functions for maxLINQ instances
		maxLinqMan.helpFN \"<function name>\": shows the specific help and examples for the maxLinq function
		maxLinqMan.helpFN \"LA\": shows help for LambdExpressions syntax
		maxLinqMan.helpMAN(): shows this message for help on MaxLinq Manager Functions
		maxLinqMan.check \"<Query Function Expression>\": checks for validity of a query expression.
										-- The passed expression must be a string and must start with a Query Function name and end with a closing parenthesis ')'.
										--	You must replace dots between succesive Functions with vertical slash '|'
										--	Avoid esplicit arrays declarations (as #(1,2,3,4)) in your expression for checking
									/*
									Example 1:
													maxLinqMan.check \"mappedGeometry(\\\"i,key,q: SelectedVerts v => v.z <= 0\\\"))|Sum(\\\"s:\\\")\"
									Example 2:
													maxLinqMan.check \"mappedprop(#'q:verts v=>true')
																	)|GroupBy(#'q,i,s,key:v=>(v.pos.z / 10) as integer')
																	)|_where(#'q,s:vset=>abs (mod vset.key 2) == 1')
																	)|Apply(#'obj,s: vset=>setVertSelection obj vset.value keep:true')\"
									*/
		maxLinqMan.isInstance <qarray>: checks if <qarray> is a maxLinq instance. 	Returns TRUE if qarray is a maxLINQ instance, FALSE otherwise.
		maxLinqMan.dispose <[&]qarray>: Frees the qarray maxLINQ instance array (if qarray is a maxLINQ instance). If '&' parameter, qarray variable is set to undefined
		maxLinqMan.createInstance <array value>: returns a maxLINQ instance with 'var' value. 'var' must be an array or a mappable maxObject
		maxLinqMan.ToArray <qarray>: if qarray is a maxLINQ instance, returns its array value (Same as qarray.ToArray() method).
	")		
				print helpmsg
				ok
			),
			
			on create do
			(
				compileIndexSort()
			)
		)
		::maxLINQMan = maxLINQManager()
	--valid--)
	
OK	
)	
	
	
	
	
	
	