namespace Library
open System

module Day2 =

    let processInput (input : string) =
        let values = input.Split([|", "; "\n"|], StringSplitOptions.RemoveEmptyEntries)
        values |> Seq.toList       
    
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
                            |> Seq.map(identifyId)
                            |> Seq.fold(updateTypeCount) (0,0)
        pairs * triplets


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
