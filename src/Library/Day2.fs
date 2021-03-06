namespace Library
open System

module Day2 =

    let processInput (input : string) =
        let values = input.Split([|", "; "\n"|], StringSplitOptions.RemoveEmptyEntries)
        values |> Seq.toArray       
    
    type IdType =
    | HasPair
    | HasTriplet
    | HasBoth
    | HasNeither

    let countChar (counts : Map<char,int>) (c : char) =
        match counts.ContainsKey c with
        | true -> counts.Add(c, counts.Item(c) + 1)
        | false -> counts.Add(c, 1)

    let getIdType (counts : Map<char,int>) : IdType =
        let values = counts 
                    |> Map.toSeq 
                    |> Seq.map(fun (k, v) -> v)
        let hasPair = Seq.contains(2) values
        let hasTriplet = Seq.contains(3) values                        
        match (hasPair, hasTriplet) with
        | (true, true) -> HasBoth
        | (true, false) -> HasPair
        | (false, true) -> HasTriplet
        | (false, false) -> HasNeither          


    let identifyId (id : string) : IdType =
        id
        |> Seq.fold(countChar) Map.empty<char,int>
        |> getIdType

    let updateTypeCount ((x,y) : (int*int)) (idType : IdType) : (int*int) =
        match idType with
        | HasBoth -> (x+1, y+1)
        | HasPair -> (x+1, y)
        | HasTriplet -> (x, y+1)
        | HasNeither -> (x,y)
    
    let part1 input =                 
        let ids = processInput input
        let (pairs,triplets) = ids
                            |> Array.map(identifyId)
                            |> Array.fold(updateTypeCount) (0,0)
        pairs * triplets

    type AlphabetNode =
    | Node of Map<char, AlphabetNode>
    | Leaf of int
    | Empty

    type Result =
    | KeepGoing of AlphabetNode
    | FoundOffByOne of int

    let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

    let checkAndAddChars str ((a,b) : char*char) =
        match a = b with
        | true -> str + string a
        | false -> str

    let findSharedLetters a b =
        a |> Seq.zip(b)
          |> Seq.fold checkAndAddChars ""

    let atEndOfId comparisonNodes i =
        match comparisonNodes with
        | [] ->
            i |> Leaf |> KeepGoing
        | first::_ ->
            match first with
            | Node _ -> i |> Leaf |> KeepGoing
            | Leaf j -> FoundOffByOne j        
            | Empty -> i |> Leaf |> KeepGoing

    let getNextCompareNode ch node =
        match node with
        | Node m -> 
            match m.ContainsKey ch with
            | true -> m.[ch]
            | false -> Empty
        | Leaf i -> Empty
        | Empty -> Empty

    let getNewComparisonNodes ch compareNodes =
        let mapping = getNextCompareNode ch        
        compareNodes |> List.map(mapping) |> List.filter(fun n -> 
            match n with
            | Node _ -> true
            | _ -> false
            )

    let addCurrentNodeComparisons node comparisons =
        match node with
        | Node n -> 
            n
            |> Map.toList
            |> List.map(fun (_, v) -> v)
            |> List.append(comparisons)
        | _ -> comparisons            

    let rec addIdToTree (id : char list) node (comparisonNodes : AlphabetNode list) i =
        match id with
        | [] -> atEndOfId comparisonNodes i
        | first::rest ->
            match node with
            | Node m ->
                let newComparisons = getNewComparisonNodes first comparisonNodes
                let newComparisons = addCurrentNodeComparisons node newComparisons                
                let result = match m |> Map.containsKey(first) with
                    | true -> addIdToTree rest m.[first] newComparisons i
                    | false -> addIdToTree rest Empty newComparisons i
                match result with
                | KeepGoing n -> m.Add(first, n) |> Node |> KeepGoing
                | FoundOffByOne j -> result
            | _ ->
                let newComparisons = getNewComparisonNodes first comparisonNodes
                let result = addIdToTree rest Empty newComparisons i
                match result with
                | KeepGoing n -> 
                    let newMap = [for ch in alphabet -> ch, Empty] |> Map.ofSeq
                    newMap.Add(first, n) |> Node |> KeepGoing
                | FoundOffByOne _ -> result

    let rec printNode node i =
        match node with
        | Node n -> n |> Map.toSeq |> Seq.iter(fun (k, v) -> 
            match v with
            | Node _ ->
                let tabs = String.replicate i "\t"
                let content = k.ToString()
                let content = tabs + content
                printfn "%s" content
                printNode v (i + 1)
            | Leaf i ->
                let tabs = String.replicate i "\t"
                let content = k.ToString() + " " + i.ToString()
                let content = tabs + content
                printfn "%s" content
            | Empty -> ()
                )
        | _ -> ()         

    let rec findOffByOneIds ids node i (origIdList : string[]) =
        match ids with
        | [] ->
            ("", "")
        | first::rest ->
            let _ = match i = 2 with
            | true -> () 
                //printNode node 0
            | false -> ()
            let result = addIdToTree first node [] i
            let nextIndex = i + 1
            match result with
            | KeepGoing n -> findOffByOneIds rest n nextIndex origIdList
            | FoundOffByOne j -> (origIdList.[i], origIdList.[j])


    let part2 input =
        let ids = processInput input
        let charList = ids |> Array.toList |> List.map(fun s -> s |> Seq.toList)
        let startNode = [for ch in alphabet -> ch, Empty] |> Map.ofSeq |> Node
        let (id1, id2) = findOffByOneIds charList startNode 0 ids
        findSharedLetters id1 id2

    let inputStr = "wxlnjevbfozadyiqpuzkrhstkg
wxlsjivbfodamyiqpuzcxhstkg
wxlnjevbfouammnqpuzcrhstkg
wxlnjevbfobwmyiqpuzprhstkg
wxlnjefbfodamyiqpuzcnustkg
wxlnjevbfodakyyupuzcrhstkg
wxlnjejbfodafynqpuzcrhstkg
wxlnjevbfodomyifptzcrhstkg
wxlnjevbfodamyiwcuzcrhstkz
wxlnjevbfofamyiqpuznrhstxg
wxlnjevbfodamyiqbupcrxstkg
wxjnjevbfodxmyeqpuzcrhstkg
xwlnjevbfosamyiqpuzcrhstkg
fxlnjevbfodrmyiqpuzcrbstkg
wxlnjevpfodamyiqquzzrhstkg
wwlnjenbfodawyiqpuzcrhstkg
wxrnjevbfodamyiqpuzlrhstrg
welnjeqbfodkmyiqpuzcrhstkg
walnjfvbfodamyiqpuzcrhwtkg
wdlnjevbfodamriqpuzjrhstkg
wxlnjevbfodmnyizpuzcrhstkg
wxlnjevbfodgmyiqpuxcrjstkg
wxlnjevbfkdamyiqpudcrestkg
wxlyjevbfodamyiqpuzcehstkj
wxlnjevamodamyiqpuzcrhatkg
fxlnqevsfodamyiqpuzcrhstkg
wqlnjevbfodanyiqvuzcrhstkg
wxlnjevbfoiamyzqpuzcrnstkg
wxlnjevbfodamyiqpuacrhsjig
wxlnjuvbfodzmyvqpuzcrhstkg
kxlnjevbfolamviqpuzcrhstkg
wxlnjesbfldamyiqpuycrhstkg
nxltjevbfodgmyiqpuzcrhstkg
ojlnjevbfooamyiqpuzcrhstkg
wxlnjevbfodaffiqpugcrhstkg
cxlnievbfodamyuqpuzcrhstkg
wxlouevbjodamyiqpuzcrhstkg
wafnjevbfhdamyiqpuzcrhstkg
wxlnjevbfxdamrpqpuzcrhstkg
wxlnjepffodamyiqphzcrhstkg
wxlnkevbfohamciqpuzcrhstkg
wzlnmevdfodamyiqpuzcrhstkg
wxlzjtvbfodamyiqpuzcrhstkd
gxlnjehbfojamyiqpuzcrhstkg
wxlnjeoqfodamyiqprzcrhstkg
nxllvevbfodamyiqpuzcrhstkg
wxlnjevtfomamyiqpurcrhstkg
sxlnjevafodamyikpuzcrhstkg
wxlnjevbfodamyfqpuzcyhztkg
wxlnjevbfodamyiqpulnrhstkh
wxlnwevbfodumyiqpuzqrhstkg
wxldjevbfodamyiqpzzcrhstkk
jxlnjevbfodamyiqphzcrnstkg
fxlnjeibfodcmyiqpuzcrhstkg
wxlnjevufodamyiqpnzcrhstkk
wglnjevbfodamyiqpuzcesstkg
wxlvjevbdodamyiqpuzcrhstkc
wxlnjevbfodabyicpuzcrhstkl
wxlnjevbfodamyiqpizcrhstvt
wolnjevbfodawyiqiuzcrhstkg
wxlyjevbfodamyuqpxzcrhstkg
wxlijevbfodamyikpuzyrhstkg
wxennevbfodamyiqpuzcrtstkg
wxlnjevbyodamyuqpwzcrhstkg
wxlnjevbfoiomyiqpuzcrhsteg
wxlnjehbrodamyiqpuicrhstkg
xxlnjevufodamyiqbuzcrhstkg
wxlojevbfodamyiqpezcrhatkg
wxljjevbfolamuiqpuzcrhstkg
wxlnjevbfodamyiqruzcrhstpi
wxlnjevbfomamyiqjnzcrhstkg
wxlnjevbfodahyiqzuzcrhstpg
wxtnjevbfodamyiqpuzcrhsdrg
wxlnjevbfodamynrpuzcrhstkz
wxlqjevefqdamyiqpuzcrhstkg
wxlnjevbfmdamyiqnuzckhstkg
wxlnjevbfodajyiqprzcrjstkg
wxlnjqvbhodamyidpuzcrhstkg
wxlnjhvbfodamriqpuzcchstkg
wglnjevbfodamyiqpupfrhstkg
wulnjevdfodamyiqpuzcrhsteg
vxlojevbfodamyiqpuzcrhstsg
wxlnjvvbfodamiiqpuzcrhttkg
wxlnjevbfodabyiqpuzzrhetkg
wxhnjevbfodamyiqpuwcrsstkg
wslzjbvbfodamyiqpuzcrhstkg
rxlnjevbfodhmyiqpupcrhstkg
wxlnjevbfhdamyiqpuvcrhskkg
wxlrjevbxodamyiqpuzcrhstag
wxlsbevbfodammiqpuzcrhstkg
wxlnjzvbfodemyiqpmzcrhstkg
wxlnoevbfodgmyiqpuzbrhstkg
wxlnjefbfodamyinpuzcrhwtkg
bxlnjevbfwdamyiqpuocrhstkg
cxlnjevbjodamyiqpuzcrhslkg
wflnjevbforemyiqpuzcrhstkg
wxlmjeoboodamyiqpuzcrhstkg
wxlnjevbfadaiyiqpuzcrhutkg
wxlnmevbfodamyyqpuzcrjstkg
wxlnjovbfodamyippjzcrhstkg
wxlnjmvbfodamyiqpszcrhsbkg
wxlnjeebfodamyicpuxcrhstkg
wxlnrehbfodamyiqpuzcrhytkg
wxlnjevbfogamyiqwurcrhstkg
wxlujevbnodamyiqpuzcrhstng
wxlnoenofodamyiqpuzcrhstkg
wxsnjevbfsdamyiqsuzcrhstkg
wxlnjevwfodamyiqpuzxrhqtkg
wxlnjevbnodamyiqxulcrhstkg
wxlijetpfodamyiqpuzcrhstkg
wxlnjzvbfidamyiqpuzcrbstkg
wxlnjevefodavyiqpuzcthstkg
wxlnjevbfozamyiqpurcrbstkg
wxlnjfvpfodamyiqpuzcrhntkg
wxlnjevbfvdamyiqvuzcrhqtkg
wilejevbfodamyilpuzcrhstkg
wxlnhevbfodamtiqpuzcrhstke
wxlwjevbfodahyiqppzcrhstkg
wxlnjevbfodamyuqpuzwrrstkg
xxsnjevbfodamyiqpuzcrhstkl
wglnjevbdodamyaqpuzcrhstkg
wxlnjefbwodamyiqpuzcrhsykg
wxwnjevbfodamyiqpuzcrhpckg
wxlnjuvbfidamyiqpuzczhstkg
wxlnzhybfodamyiqpuzcrhstkg
wxunjevufodamyiqpuzcrhspkg
wxunjevbfodcmyiqpuzcrhstcg
wxlnjevbfodhwyiqpuxcrhstkg
wxlnjevtfodasyiqpuzcrhstkb
wxlvjevbfqdamyiqprzcrhstkg
sxlnjevbfodamyiqplzcrhstkq
wxlnlevbfodamyiqpuzcrpstka
wxlnjevbfodaiyiqovzcrhstkg
wxlntevbfodamyiqpuzcrkntkg
wxlnjevbfodsmyiqpuzcrhstir
wxlnnevbfodaoyiqpuzmrhstkg
xxlnjevbfodamyinpnzcrhstkg
wxlnjedefodamyigpuzcrhstkg
wxlnxeabfodamyiqpnzcrhstkg
wxlnxevbfodpmtiqpuzcrhstkg
wxlnjevnfodamyiqpuzcuhqtkg
wxlnjevbfodakyiqluzcrhstmg
wxlnjevbaodamoiqpyzcrhstkg
wwlnjevbfoaajyiqpuzcrhstkg
wxlnjevbfedamyiqpuzcrhsang
wxlwjevbfodamyiqpuzcrmdtkg
wxlnjevbhodamyiqpmzxrhstkg
wxlnjevbfodamyiqpuzzrhwtkj
wxlnjevbfpdvmyiqpuzzrhstkg
wxlnjegcfodamyiqpxzcrhstkg
fxlnjevbfodamyiqpuzcrhstat
wxlnjevbfodcmybqpuzcrkstkg
wxlnjevbfodamiiqpuzrrhstxg
wxvnjevifodamdiqpuzcrhstkg
wxltjevbfodamyiqpuzmrhktkg
wxlnjevbfobaaygqpuzcrhstkg
wmlnjevbfodamyiqpuycrhsukg
wxlnjevboodamyiqpuzcrhuhkg
wxlnjevgfodaqyiqpuzcghstkg
wxlnjevjnodamyiqpuzcrhstke
wclnjevbfodamyiqpuncchstkg
wxlnjevbfndamyxqpuzcshstkg
rxldjevbfodamyiqpuvcrhstkg
wxlnwevbfodamywqpuzrrhstkg
ixlnjevbqodpmyiqpuzcrhstkg
wxlnjlvbfocamyiqpuzgrhstkg
wxlnjevffodamyiqnuzcrhutkg
wxlajevbfodamyiqpuccrhshkg
vwlnjevbfodamyiqpuzcrhstky
wxlajevbfodamyiqpuzcfhstkl
wxlnjevbfodamniqouzcrhstko
wxlnjevbfodamyiqpuzqrhsqka
wxlnjeybfodamyiqpuzclhsnkg
wxbnjlvbfoyamyiqpuzcrhstkg
wxbnjevbfodemyiqpuzcrhstkj
wxlnbefbfodamyiqpkzcrhstkg
wxlnjvvbyodamyitpuzcrhstkg
jxlnjevbfopamyiqpuzprhstkg
wxlnjevbfodaeyiupxzcrhstkg
wnlnjevbfodamyqqpuzcrhstcg
wxlxzuvbfodamyiqpuzcrhstkg
wxlnjevbcodymyiqpuzcrhstke
wxlnjezbfodamynqpuvcrhstkg
wxlnjevbfodamyxlpuzcyhstkg
wxlnjevbffdaiyiqpuzirhstkg
wxlnjevbfodymyiqwuzcrhstfg
wxlnzevbfodscyiqpuzcrhstkg
hxlnjevbfodamyawpuzcrhstkg
welnjevbfodamciqplzcrhstkg
wxlnjeqbfodawyiqpuzkrhstkg
wxlnjelbfodamviqpuzckhstkg
wxlneevjfodamyiqpuzcrhstkd
wxlnjevbfodamyaqpuytrhstkg
wxlnjpvyfodamyiqpuzcshstkg
wxlnjevbfodmbyiqpuzcrhstkp
wxlnjegbfodamdiqpuzcrhstdg
wmlnjevbfodamyiqpuecrhsukg
wxlnjevbfodamyiqpuocrhwtjg
jxfnwevbfodamyiqpuzcrhstkg
wxlnjevffodamyiqpurcrhstkd
wxlnjevbfofamyiqpuzcrhsmkt
wxlnjevbfodmmyiqpuzcrdsttg
axlnjevbfodgmyiqpuzerhstkg
wxtnjevbfodamyifpuwcrhstkg
wxlgjevbfodamyiqpuzrrhvtkg
wxlnjevbfouamyeqfuzcrhstkg
wxlnjevbfmxamyiqpuzcahstkg
wxlnjevffoxamyiqpuecrhstkg
wxlnyevbfodamyiqttzcrhstkg
bxlnjevbfodzmysqpuzcrhstkg
wxlnjevbfodayyiqpuzcrherkg
yxlnjevbfodayyiqpuzcrwstkg
wllnjevbfodambiqpuzurhstkg
wxlnjevbfsdamyiqpuqcrhstkh
wxlcjevbfodamyiqcuzcxhstkg
wxlnjevbfodayticpuzcrhstkg
wxltzevbfodamyiqpzzcrhstkg
wxlnjevbfodamgiqpuzcphstng
wxlnjevbfqdfmziqpuzcrhstkg
wxlnaevbfodamyiqpuzcfustkg
wxlnjevbfodamyxqxuzcrhstdg
wxlnjevkbodamyiqpufcrhstkg
whlnjevbfodauyiqputcrhstkg
wxlnjevbfodamyiephzcrhsnkg
wxlnjevbfodfmoiqpuzcrhstkf
wxlnjevbfodamyiqxuzaxhstkg
wxlnjevtfotamyiqpuzcrhsttg
wxlgjevbfodamyiqpuhcrostkg
dxlnjtvbfodamyiqpuzcshstkg
wxlfjevbfodumyiqppzcrhstkg
wxlnzevbfodamyiqpuzqrhstkx
wflnjevbfodamyiqpurcrhsthg
wxlnjevbfodzfyiqpuzcrjstkg
wxlnjevbfrdamviqpuzmrhstkg
wnlngevmfodamyiqpuzcrhstkg
walzjevbfodamyiqpuzcrhsjkg
wqlnjevbfodamyiqpuzcshslkg
wxlnjevkfodfmyiepuzcrhstkg
wxgnjehbfodamyhqpuzcrhstkg
wxlnjevbfodamyiqfuacrostkg
wxlnjexbfodamyiwpuzcrqstkg
wxlntevafodamyiqpuzcrhsnkg
wxvnjevbfodamyiqpuzcvistkg
mxlnjeebfodamyiqpuzcrhsgkg
wxlnjevyfodamyiqpuzcrhdtkf
wxlnjcvbfodamyicpuzcrhsckg
wxlnjekbfodlmyiqpuzcthstkg
wxlnjvvbfodamyiopuzcrhstqg
wxlnjevbsodamyiqpuhcrhstwg
wxxnjevufodamyiqruzcrhstkg"

    let result1 = part1 inputStr
    printfn "%i" result1
    let result2 = part2 inputStr
    printfn "%s" result2
