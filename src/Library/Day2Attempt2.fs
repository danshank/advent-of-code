namespace Library
open System

module Day2 =

    let processInput (input : string) =
        let values = input.Split([|", "; "\n"|], StringSplitOptions.RemoveEmptyEntries)
        values |> Seq.toList    

    type ErrorState =
    | Continue0
    | Continue1
    | Exit

    type IsOffByOne =
    | FoundIdentical of string
    | FoundOffByOne of string*string
    | DidNotFind

    let rec CheckListForOffByOne id check =
        let tupToInt (x,y) =
            match x=y with
            | true -> 0
            | false -> 1

        match check with
        | [] -> DidNotFind
        | first::rest ->
            let isEqual = id |> Seq.zip first |> Seq.sumBy(tupToInt)
            match isEqual with
            | 0 -> FoundIdentical id
            | 1 -> (id,first) |> FoundOffByOne
            | _ -> CheckListForOffByOne id rest


    let rec findOneOffIds ids check =
        match ids with
        | [] -> ("", "")
        | firstId::restId ->
            match check with
            | [] -> findOneOffIds restId [firstId]
            | _ ->
                let result = CheckListForOffByOne firstId check
                match result with
                | FoundIdentical s -> (s,s)
                | FoundOffByOne (a,b) -> (a,b)
                | DidNotFind -> 
                    let newChecks = firstId::check
                    findOneOffIds restId newChecks

    let checkAndAddChars str ((a,b) : char*char) =
        match a = b with
        | true -> str + string a
        | false -> str

    let findSharedLetters a b =
        a |> Seq.zip(b)
          |> Seq.fold checkAndAddChars ""


    let part2 input =
        let ids = processInput input
        let (a, b) = findOneOffIds ids []
        findSharedLetters a b

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

    let result2 = part2 inputStr
    printfn "%s" result2    