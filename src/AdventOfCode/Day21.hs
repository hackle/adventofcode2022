module AdventOfCode.Day21 where

import Text.Parsec
import qualified Data.Map as M

data Op = Plus | Minus | Multi | Div deriving (Eq, Show)
data Expr = Lit Int | Fn Op String String deriving (Eq, Show)

fn :: Op -> (Int -> Int -> Int)
fn op = case op of
            Plus -> (+)
            Minus -> (-)
            Multi -> (*)
            Div -> div

eval :: M.Map String Expr -> Expr -> Int
eval ms (Lit n) = n
eval ms (Fn op v1 v2) = (fn op) (eval ms $ ms M.! v1) (eval ms $ ms M.! v2)

type Parser = Parsec String ()

pOp :: Parser Op
pOp = do
    c <- oneOf "+-*/"
    case c of
        '+' -> pure Plus
        '-' -> pure Minus
        '*' -> pure Multi
        '/' -> pure Div

pLit :: Parser Expr
pLit = do
    n <- many1 digit
    pure (Lit (read n :: Int))

pFn :: Parser Expr
pFn = do
    v1 <- many1 letter
    op <- char ' ' *> pOp <* char ' '
    v2 <- many1 letter
    pure $ Fn op v1 v2

pLine = do
    k <- many1 letter <* string ": "
    fnOrLit <- pLit <|> pFn
    optional endOfLine
    pure $ (k, fnOrLit)

pInput = many1 pLine

solve :: String -> String
solve raw = 
    case parse pInput "{unknown}" raw of 
        Left err -> show err
        Right exprs -> 
            let ms = M.fromList exprs 
            in show $ eval ms (ms M.! "root")

testInput = 
    "root: pppw + sjmn\n\
    \dbpl: 5\n\
    \cczh: sllz + lgvd\n\
    \zczc: 2\n\
    \ptdq: humn - dvpt\n\
    \dvpt: 3\n\
    \lfqf: 4\n\
    \humn: 5\n\
    \ljgn: 2\n\
    \sjmn: drzm * dbpl\n\
    \sllz: 4\n\
    \pppw: cczh / lfqf\n\
    \lgvd: ljgn * ptdq\n\
    \drzm: hmdt - zczc\n\
    \hmdt: 32"

prodInput = 
    "nprq: 2\n\
    \wvzt: 5\n\
    \mlgg: 2\n\
    \lnrg: msgc + slpg\n\
    \dbmv: 5\n\
    \zpmd: 6\n\
    \qmvp: llms + rmjj\n\
    \llsn: 11\n\
    \dppq: 2\n\
    \vnqj: 4\n\
    \lpdz: jznp * rjft\n\
    \pdmh: jscl * plbn\n\
    \thvd: wzjp / dppq\n\
    \stpf: 1\n\
    \bmqc: hjbc - rfmb\n\
    \dgjc: jzlg * sbzb\n\
    \gfbb: 5\n\
    \vrjz: 5\n\
    \rmgd: 1\n\
    \tfmm: rhwf * btbr\n\
    \zhsf: chzv + nwrh\n\
    \rnwc: hgmm + nrtw\n\
    \bgsm: glsf + hcwp\n\
    \vwqd: 4\n\
    \qvvg: qhcg + stpf\n\
    \cqpd: bhwg + stwf\n\
    \wprl: 4\n\
    \cvzv: lnbg * sstb\n\
    \qdrz: dfdq * wcjn\n\
    \dszj: jvfw * rwwz\n\
    \lfnz: mfvg * vvbm\n\
    \chzq: mrzl - zjnc\n\
    \rmhv: zfgv + pqjl\n\
    \hfsf: wshc / hwrw\n\
    \tpmw: 17\n\
    \fzgj: 3\n\
    \jjjr: zdcm - rrbj\n\
    \dbfh: bcmr + zgrc\n\
    \hcfd: 4\n\
    \hjwg: 4\n\
    \fbbz: qpbw + phpl\n\
    \gwzj: sngg * nttl\n\
    \jvdc: vbwq * hqfz\n\
    \thgv: 3\n\
    \qlgm: jzlr + dqjc\n\
    \pfzr: nqzq - qnlb\n\
    \lljm: lbft * dtmz\n\
    \ghdb: 5\n\
    \scss: vvnj - hfsq\n\
    \zmmp: 3\n\
    \tgww: 10\n\
    \csgb: vwwh + qbhv\n\
    \vdms: 7\n\
    \lmfg: 13\n\
    \npjd: rdnt * bwwh\n\
    \cvsl: 2\n\
    \fjbm: 1\n\
    \dfzr: 2\n\
    \wbwz: 5\n\
    \qstp: 16\n\
    \jzjj: jnlc + zgsq\n\
    \nzvh: nwdd + hlsq\n\
    \qpcn: jggg + bttn\n\
    \njhv: mvrm * zsmq\n\
    \gjch: nctt + srbn\n\
    \lzgm: 12\n\
    \ddmh: 2\n\
    \qttr: 15\n\
    \jwlc: qfrj * tzgl\n\
    \cjns: bhqn - rnhm\n\
    \gcvd: flcm * phzv\n\
    \vrht: fhph + csqs\n\
    \jcfl: rwcf * hdtt\n\
    \dvzw: 2\n\
    \hfps: dncr + mdjs\n\
    \sbhv: pzsq - dwvp\n\
    \tfrb: gfwz * pjrt\n\
    \cwsq: hnln * qbpc\n\
    \srbn: llcm + jjth\n\
    \rfdf: nwvb + cfgs\n\
    \fvqc: 6\n\
    \jzzq: 1\n\
    \nfqw: 3\n\
    \wwzn: 5\n\
    \rprl: 3\n\
    \mdvn: 2\n\
    \vcpv: 2\n\
    \pmqb: vfcv * bgcf\n\
    \pzzf: pvcc * jrvm\n\
    \dvmd: qtlb * vbwv\n\
    \bcvw: trmr * hptb\n\
    \bbsh: hghq + qmrl\n\
    \cwlg: mghp - wprl\n\
    \tqrw: zbtl * jhgl\n\
    \hczn: 2\n\
    \lvdv: 7\n\
    \sfvv: 13\n\
    \vbbm: 5\n\
    \jfrl: 5\n\
    \tbjj: 3\n\
    \qqqq: 5\n\
    \wrdb: 13\n\
    \bbpv: gbdn * qjpw\n\
    \ccwj: hznc * smfh\n\
    \wwnf: nmcn + wjqq\n\
    \htrq: 3\n\
    \dpmz: tvvc + tswl\n\
    \sgql: wdff * scss\n\
    \vwzb: 2\n\
    \gdmq: 7\n\
    \wjfb: fcmm * ghqb\n\
    \jhgl: 7\n\
    \jrzm: nzvh - dzcc\n\
    \lrph: 3\n\
    \jfwb: 4\n\
    \jzlg: 3\n\
    \ctzp: lvvf * dbbq\n\
    \cjfh: cjbs + bpsw\n\
    \ngzt: 8\n\
    \nrrd: vrnh + rcbr\n\
    \pnbf: 13\n\
    \gsjs: zdnf + qnft\n\
    \wwzt: 3\n\
    \gmgs: qqlz + jnjd\n\
    \rgzt: 8\n\
    \wbdq: 3\n\
    \tgsj: wzvv + cnmz\n\
    \dpwl: 4\n\
    \jbbm: cslc * rjtn\n\
    \nwrh: vhlm * hpmm\n\
    \jgmj: 4\n\
    \qwwl: 2\n\
    \fcfd: ggtd * lnln\n\
    \shfh: 4\n\
    \vfnh: fmdq * qctz\n\
    \rpst: pqmw + hpbz\n\
    \drwl: rgzt + tpvg\n\
    \tcjr: 2\n\
    \cppz: tscr * dhpq\n\
    \fmrv: drjj * tftr\n\
    \cllv: zpbt - cnsw\n\
    \rjcw: rdhh * zcvz\n\
    \zrbz: 11\n\
    \dfhw: 2\n\
    \bvtf: 11\n\
    \qmqb: bdht * hbmg\n\
    \rqrh: 11\n\
    \fjgs: fhsp * rjqq\n\
    \nddc: djcd * gvzz\n\
    \sznc: 2\n\
    \jsft: rrcs / gpjj\n\
    \pswm: rmcj * bndm\n\
    \fhdh: mgrn * fnqt\n\
    \ltdc: 3\n\
    \mdtt: fvwh - ptcq\n\
    \fcpd: 3\n\
    \hnmz: 3\n\
    \bhcs: ncnl + jrrg\n\
    \mnqr: 3\n\
    \pdnf: jbbm / dpbt\n\
    \hqhr: 16\n\
    \brgr: 3\n\
    \hsqh: 4\n\
    \dtsp: gcnn * cbbv\n\
    \hnln: psct / vjsj\n\
    \vvzw: 2\n\
    \clgt: 5\n\
    \ldlm: hfps + pvjd\n\
    \qdmg: vpvz + vsrb\n\
    \wmbz: 11\n\
    \fnqt: 2\n\
    \bjfr: 1\n\
    \dqjw: 2\n\
    \dprl: thvw + cmmf\n\
    \tsmq: wwnf / lqnz\n\
    \tgdw: 7\n\
    \sdhs: ddqs + sjvq\n\
    \msqm: 2\n\
    \sfmq: 3\n\
    \ngch: fhdh + zqnm\n\
    \rpjn: vbwf + tlqn\n\
    \wttd: hvss * dmpw\n\
    \strj: zmzs + vqlc\n\
    \zrjr: jrzm - dlwj\n\
    \znbf: dngb * tvgl\n\
    \pbcg: 5\n\
    \lmgn: 9\n\
    \zhhd: qdlt * vjct\n\
    \zvrp: 7\n\
    \pfrp: lnpn + ngnz\n\
    \nvfl: 2\n\
    \gzfv: 4\n\
    \ctlr: gqvq * nnjh\n\
    \mwfb: 2\n\
    \vfmz: 2\n\
    \sldq: 4\n\
    \lvcf: 8\n\
    \gzpd: 12\n\
    \lnbg: bmcv + dbjm\n\
    \wnsl: dhrb + lgsn\n\
    \mgpp: 8\n\
    \dsvn: pvtt * zscw\n\
    \jvgp: 2\n\
    \zdpt: 4\n\
    \nttl: fgbh + vzzl\n\
    \zhdc: 3\n\
    \wdfb: 7\n\
    \llcm: 1\n\
    \jntg: 3\n\
    \lnpn: dbtc * mgpp\n\
    \cdlf: hdns + nsqz\n\
    \ntcb: lcjs + llgf\n\
    \tdrr: 10\n\
    \lhfm: 9\n\
    \qwbp: 4\n\
    \tgbb: 3\n\
    \ppnz: 3\n\
    \jnfj: wzqw + scpp\n\
    \fpfc: 3\n\
    \jdcj: 2\n\
    \mllp: qzhz * dgfm\n\
    \dbsv: 4\n\
    \dngb: bldq * pbcg\n\
    \nnpw: 4\n\
    \nhvb: gwpt * lmts\n\
    \mqzg: ljhw + brss\n\
    \vcfg: nnzl * gwzz\n\
    \zphl: 1\n\
    \tmnp: mhnw * lqpd\n\
    \drgg: 5\n\
    \fsll: tcvc + nnfj\n\
    \qmfl: mflw / bbwc\n\
    \rtdl: 6\n\
    \tbzv: gsfv + hbmn\n\
    \hjlz: 2\n\
    \dlwm: tggn * pjqs\n\
    \qpgs: 2\n\
    \lzvq: dszj + jrdn\n\
    \dbtp: 2\n\
    \cmpb: 3\n\
    \lrmd: flvp + wzfz\n\
    \qpzb: rtlc * hzdw\n\
    \tnrc: wcwc + shfh\n\
    \pdhm: 1\n\
    \jzvb: ggmj + mbpn\n\
    \vdjv: zgvb * qdnl\n\
    \zfgv: fznv * dhbh\n\
    \cjll: 18\n\
    \mwwq: 2\n\
    \swzv: 5\n\
    \fwgm: cztz * rgjs\n\
    \jwrt: 3\n\
    \vdcg: wwfm / qtqp\n\
    \tncj: 2\n\
    \fnpz: 3\n\
    \zsdv: qwwl * bgsm\n\
    \brcc: 3\n\
    \ssrg: 5\n\
    \plvj: 10\n\
    \wtbn: tcjn + nchh\n\
    \tmvj: 1\n\
    \rdmc: zcjz + rnwc\n\
    \fzqb: zscf / nmhw\n\
    \gwjq: 3\n\
    \qfgs: jmvz + lmgn\n\
    \jmqt: qdrz * wmzl\n\
    \clbz: jzhl * vrht\n\
    \jwqj: 2\n\
    \bvwd: 13\n\
    \vpnr: dfzr * zrhr\n\
    \gmcn: rbsz * lbgm\n\
    \gzjp: 3\n\
    \ftlc: 5\n\
    \tcdf: 11\n\
    \nhgm: lrqt + nlfz\n\
    \qjzs: 16\n\
    \lqvj: 2\n\
    \tjdw: 5\n\
    \vrvz: zbhb + pbzj\n\
    \cqrm: rblq * hgnm\n\
    \vhlm: 2\n\
    \qjpw: hjwg + ftcz\n\
    \ltnn: 7\n\
    \mcqr: wvgb * pbcq\n\
    \zpbt: dcpz + cjcw\n\
    \wdwc: nvth * bqml\n\
    \sprb: dlmb * cndq\n\
    \nzzr: 3\n\
    \lwpb: 3\n\
    \hzzz: 2\n\
    \twwp: fmmh - ccgr\n\
    \lpzb: bdjh * rpzz\n\
    \bhhr: dpbj / nvzq\n\
    \dgvd: rtzw * fljw\n\
    \smvr: 1\n\
    \cmdd: 2\n\
    \tcdp: 7\n\
    \bttn: 4\n\
    \gfwj: twtq + mjmc\n\
    \gwpt: bzzw + zpmr\n\
    \tltd: 7\n\
    \mdjs: 5\n\
    \tldp: bzpc + tdln\n\
    \rwzw: 4\n\
    \rqmh: cnbn * lnrp\n\
    \nrgr: 3\n\
    \cfgs: 13\n\
    \dqjc: spbs + swnv\n\
    \vhph: dttr + npcq\n\
    \djcs: 3\n\
    \tltr: 7\n\
    \clzb: 2\n\
    \fvhw: 13\n\
    \psrd: qlgm * vdqf\n\
    \tdlb: brjh + twtz\n\
    \jpnn: 8\n\
    \chtj: 3\n\
    \wdqh: tldp + lzpl\n\
    \shqt: ttgf + vlpn\n\
    \mjsn: 16\n\
    \qfwb: tpvf + cmnd\n\
    \plcd: mfjg + nlhw\n\
    \vszj: cdgz * rhll\n\
    \nwnt: 3\n\
    \zggm: 11\n\
    \scqr: nbvc + lhzb\n\
    \clfq: jtgr - dtrd\n\
    \ztlr: nbfb * tdgs\n\
    \ftdq: 3\n\
    \lfmd: 4\n\
    \cwsh: 2\n\
    \fgdm: scnl * jvrb\n\
    \vpfq: 3\n\
    \fzfg: 2\n\
    \fhpd: 3\n\
    \rtcn: pszt + fwgm\n\
    \dtfb: wqcl * grdg\n\
    \zgrc: gvwt + qfpw\n\
    \pgwl: bmhv * jwhf\n\
    \gbdd: fcwd + qdhs\n\
    \dccn: wdfb * rpst\n\
    \rwcf: wfsd + fmzr\n\
    \nmwh: 6\n\
    \pcdb: 2\n\
    \hrpz: ctlr - tpsr\n\
    \bjcr: ldlm * jsjd\n\
    \bcmr: nflm + rvhn\n\
    \gzqv: 13\n\
    \dbzr: 2\n\
    \swrb: 6\n\
    \whpf: 3\n\
    \scdj: gzjp * vbwn\n\
    \qhss: 1\n\
    \vvqg: lttn + vfpp\n\
    \tqzj: gqhj - zpdf\n\
    \fvhf: 4\n\
    \sqsb: tctg * fzfg\n\
    \tcvc: rcnd / hpzg\n\
    \gflz: 8\n\
    \zhrd: gwjq * vnrb\n\
    \bndm: 8\n\
    \jhcn: tqzh + qplq\n\
    \djzq: 2\n\
    \bgcs: chtd - pdhm\n\
    \nfhq: 1\n\
    \dvrs: 3\n\
    \npvh: 2\n\
    \qnlb: lldz + mzfr\n\
    \jnch: zvqd * wcnl\n\
    \mhhr: dfhw * jtlm\n\
    \gcnn: fpfc + svfp\n\
    \bzsv: ghqn * zvch\n\
    \jljh: vhph * fcbn\n\
    \dhlb: jhcn + hwgn\n\
    \hrzf: tcdp * dvtc\n\
    \vbms: 5\n\
    \dtlt: 3\n\
    \vbsz: fjrn * glbt\n\
    \pbht: mwvl * vbrl\n\
    \jpjh: qpjb + zwbt\n\
    \gtmh: ljhs / jwqj\n\
    \fpgd: 2\n\
    \hltj: 5\n\
    \pzsq: 17\n\
    \vhsw: 8\n\
    \dpbt: 3\n\
    \fnsd: hncc * gnct\n\
    \ctwv: tnrc + dwhw\n\
    \hszg: lwns / ljbc\n\
    \rdmr: 8\n\
    \nbvc: 2\n\
    \prff: gzfv + tqbh\n\
    \qqtp: 2\n\
    \llpf: 2\n\
    \phhf: 9\n\
    \tlqn: zhmr * rngj\n\
    \nggd: 3\n\
    \ndls: rlfv - wwzl\n\
    \gbcm: 2\n\
    \wdch: jbqs + jgnl\n\
    \jmcc: gpbs + lnst\n\
    \dtbb: hlpd * wmzq\n\
    \hggd: vhjh + csgb\n\
    \qjvf: 5\n\
    \tcwv: 5\n\
    \nvbv: fnsd + zwhn\n\
    \cgmg: zfpm + zwzw\n\
    \sncq: ddcm - tsjd\n\
    \pbhf: qbpg / bjhl\n\
    \nvwm: qrlm * ccff\n\
    \wlmt: 2\n\
    \ccwf: 2\n\
    \qqds: cjht + vngl\n\
    \qvbd: lsms * hmfz\n\
    \zbhb: lmdt + thcm\n\
    \vthj: szhg * rjcw\n\
    \cnlm: chrc + gvnj\n\
    \jbqs: 15\n\
    \hbzc: 1\n\
    \hsbm: 6\n\
    \vfsb: tntn * hhwj\n\
    \lcjs: qdmg + sctv\n\
    \jfdb: 3\n\
    \jqdv: 3\n\
    \hhnd: hzhj * rfmm\n\
    \sdvv: pcmc + grrz\n\
    \zjqc: qpng + qqtw\n\
    \hznc: 8\n\
    \fhrp: 13\n\
    \vhjh: 5\n\
    \hmfz: 2\n\
    \tlqz: 3\n\
    \mpnv: 20\n\
    \lvvn: bpwm * vzwb\n\
    \brmj: tmvj + lffq\n\
    \dbdm: tsws * nnnt\n\
    \qqpz: lqsz * qwsv\n\
    \dmzd: qdmr * ftlc\n\
    \wcdg: 2\n\
    \dgvm: 17\n\
    \rjqq: 2\n\
    \tzbb: dgnb * plhj\n\
    \tzsj: 18\n\
    \nwzn: vtqf + sznc\n\
    \zldq: ggzw + vcqb\n\
    \dsqs: phgn * qhnh\n\
    \vjsl: 7\n\
    \qtgf: 2\n\
    \cslc: 7\n\
    \tswl: 1\n\
    \qbpg: hvgh * zhrd\n\
    \ztbc: rlqq * tlfr\n\
    \zjqb: vgdp * zpjh\n\
    \wdff: 2\n\
    \bwgw: rnlm - jqdv\n\
    \nmhz: hzlb + fstj\n\
    \jhlv: fcpd * hpfg\n\
    \rsmh: fjbm + ppzr\n\
    \jbzw: 5\n\
    \clhw: 3\n\
    \jcqj: nhcp * qvbn\n\
    \cnsw: 3\n\
    \qqtw: rqmh / fgfs\n\
    \chzv: hsgd * dlrv\n\
    \llhd: lwqf * ndjr\n\
    \sbbs: czzp * qgtc\n\
    \rhhc: jzvb * vsqd\n\
    \rblq: gwzw * zsdv\n\
    \dbbq: trdn - vfvp\n\
    \sbsg: fgfb * gwdm\n\
    \zwgw: wqqh + qfmj\n\
    \tspj: 11\n\
    \twtz: bmcs * jqfw\n\
    \hfnp: nmwh + trvn\n\
    \hzdw: 4\n\
    \svwc: 9\n\
    \jmjv: gfcd + jrfg\n\
    \blcn: nmbh - dsvj\n\
    \vvnj: 10\n\
    \nlhw: 5\n\
    \mppt: gmrc + stzf\n\
    \zvqm: rftg + lnpd\n\
    \ffhl: jhgg + flcr\n\
    \rdwq: 2\n\
    \ppwp: 6\n\
    \dcsf: 1\n\
    \jcqf: 2\n\
    \mptg: rdhv * hmgq\n\
    \tdgs: 7\n\
    \nqbh: vfnc * flmn\n\
    \mwtw: vlrn * dqjw\n\
    \wnvb: 9\n\
    \gcqt: 3\n\
    \bzfm: tfgl + wnmd\n\
    \bztd: jqfr * tltr\n\
    \gcgz: 5\n\
    \btbr: dsgd + shjz\n\
    \pdlc: pmpp - hzld\n\
    \hzhj: 15\n\
    \mvrm: 2\n\
    \qrrz: ppmj + cvdg\n\
    \scnl: 2\n\
    \ngbz: bqzh + jzjj\n\
    \vgdq: cqjp * rgwm\n\
    \brss: vdff + ntcj\n\
    \gtwt: rddc / mfqn\n\
    \jgfg: ztdl * gtjc\n\
    \grdg: 7\n\
    \hhbc: tlts + zzrs\n\
    \hcsb: 5\n\
    \ghbg: 1\n\
    \wbgn: 2\n\
    \vzbp: 3\n\
    \fstn: 2\n\
    \vplv: pbvg * mhwg\n\
    \fnnd: 3\n\
    \tggn: 2\n\
    \dcpz: 8\n\
    \hsgd: vpgd / vmjm\n\
    \qmfm: hhtp * lgrf\n\
    \wgmw: 3\n\
    \ghqb: jnvp / mvrh\n\
    \hnhg: dmzd * sfpv\n\
    \sthg: 2\n\
    \srgl: 3\n\
    \smgj: qczf / jscs\n\
    \ffqm: 5\n\
    \pmtv: 6\n\
    \vbwq: 5\n\
    \qdnl: tlgr + gdzb\n\
    \fvwh: plcd * lqsw\n\
    \bqcr: mpjj * lvgb\n\
    \ghbd: zpjm + nhvt\n\
    \zdcm: pppb + clmt\n\
    \vjsj: 2\n\
    \fmdq: lhmv + fgmr\n\
    \dhrb: bdgh + rfdf\n\
    \flnj: cmpb * tmmq\n\
    \dbgp: 20\n\
    \plhh: 2\n\
    \wwtp: ljjw + gslt\n\
    \zgcs: vgbp * jbzw\n\
    \pwff: gmtl * gwzr\n\
    \zmbh: 10\n\
    \tqmn: bwgw + hpnp\n\
    \nvzq: 4\n\
    \hwrv: hnds * smnm\n\
    \lffq: 5\n\
    \tgcb: nfwz + ztlr\n\
    \mpgc: hczn * qmvp\n\
    \mgzq: bhwh + wlnz\n\
    \fbrg: 3\n\
    \nthf: rfqc * jthc\n\
    \mfjg: 3\n\
    \vgql: 7\n\
    \qqhn: jwlc - qdqb\n\
    \dwvp: 4\n\
    \fqbv: 2\n\
    \nhlz: 13\n\
    \snjh: 2\n\
    \hqld: 13\n\
    \bbwc: 2\n\
    \sspn: 3\n\
    \sppq: 1\n\
    \vvwr: 7\n\
    \dvcj: fnpt - fsvs\n\
    \mjmj: 3\n\
    \hdns: tljl - lsjh\n\
    \fgfs: 7\n\
    \jfvz: 2\n\
    \tjwn: 2\n\
    \tgtb: pnhh * gvrb\n\
    \dgbh: jhlv / clhw\n\
    \ptql: 3\n\
    \jmvz: 2\n\
    \mzwc: rsjs + nthf\n\
    \sqwl: 3\n\
    \dgww: 2\n\
    \mtwg: fwnb * nrwb\n\
    \vfqd: dbfh + ncvt\n\
    \zlmf: 3\n\
    \sssl: 11\n\
    \zvnw: 3\n\
    \sfgc: zmbh + tfmm\n\
    \qpwf: wmqt * ffdz\n\
    \wjjt: gnzs + tnts\n\
    \pjgq: gjsh + jgvq\n\
    \dwnf: btcj * gjfr\n\
    \zglm: 2\n\
    \srvj: gclh * vjvq\n\
    \plnd: 2\n\
    \ndpr: wcql / zhdc\n\
    \hlvb: 5\n\
    \cjbs: 2\n\
    \pvtt: clbj / hjlz\n\
    \rcbr: cvmz / ldwz\n\
    \vfzp: 16\n\
    \rwbj: 3\n\
    \qczf: vrbl * twjt\n\
    \dnvh: qhgd * bnbw\n\
    \tmqg: hlqn + vvss\n\
    \zsmq: gjnw + qqhn\n\
    \vbwv: 9\n\
    \nfwz: 1\n\
    \cwtl: snds * vhvz\n\
    \glbt: 5\n\
    \nnjh: bcvw + gtwt\n\
    \glcj: 14\n\
    \nmcn: hrpz + dwnf\n\
    \fgfb: 2\n\
    \vsqd: 2\n\
    \lgrf: 2\n\
    \rcdq: qnbg * tbjj\n\
    \hvbh: 4\n\
    \drjb: 15\n\
    \bndb: 2\n\
    \tcnw: 18\n\
    \qccw: nvcl - zmjq\n\
    \ldbj: jfwb + jcls\n\
    \rffj: 13\n\
    \gdqz: 3\n\
    \wmjw: 7\n\
    \swlr: gcqt * mwfb\n\
    \vfds: lpzb + pfzr\n\
    \bnvl: 4\n\
    \dvwj: gsjs + ghnh\n\
    \tftr: 3\n\
    \bcmq: 2\n\
    \jwfs: btbt * qsqp\n\
    \cntp: 2\n\
    \vhgt: sppq + gmcn\n\
    \nqnh: 4\n\
    \ccbp: fqlw + wdch\n\
    \fwqf: sggw * mhss\n\
    \nrrp: szhv * gmgl\n\
    \jscl: 2\n\
    \bgvz: 2\n\
    \dwdd: 19\n\
    \fgbh: 2\n\
    \hstz: hwbd / twcb\n\
    \rlsw: jsft + nfhq\n\
    \sggw: fzgj * jwrt\n\
    \jzlr: 5\n\
    \sbvv: nqdb + ftwh\n\
    \djbc: ppnz * dgvm\n\
    \mfqn: 3\n\
    \vcqb: hvbh + wqsz\n\
    \cndq: 2\n\
    \pbrj: 11\n\
    \pcmc: vfsb - hznd\n\
    \ccwb: 2\n\
    \pchz: trtm * swrb\n\
    \gqvq: pzhr * jzbv\n\
    \hvss: ngch + brjv\n\
    \qfrj: 5\n\
    \cnmp: 3\n\
    \thvw: zphl + pchz\n\
    \mlwg: 3\n\
    \ddfr: qqtp + mpnv\n\
    \rzcv: 2\n\
    \wzfz: jtrl * rqwr\n\
    \nlbv: npjd + vwsn\n\
    \cnbn: jdcf * qzpm\n\
    \rhwf: 3\n\
    \szmm: nrcz * tdpc\n\
    \hrqr: 3\n\
    \zcjs: 3\n\
    \hgsr: bwqr / tcwv\n\
    \gfmp: jmwv * jwbp\n\
    \mplv: 2\n\
    \cgvz: 20\n\
    \bldq: 2\n\
    \bzww: 3\n\
    \vpvh: tfht * mfwc\n\
    \bjgg: 7\n\
    \bbts: zwjm * jtzf\n\
    \cmmf: zvrp * dzjf\n\
    \qrlm: 13\n\
    \phzv: hstn + zgpj\n\
    \vtpt: msqm * lpqw\n\
    \hhqs: 3\n\
    \rhww: 17\n\
    \svhm: 3\n\
    \blfn: 19\n\
    \mtgf: 4\n\
    \ttgf: cwsq + mwrp\n\
    \qtqp: 2\n\
    \cjzw: dvhs + tfrb\n\
    \ghrc: htvt + wgsm\n\
    \wwfm: twwp + vvqg\n\
    \hjbc: ghdm * ftdq\n\
    \flcr: rwsc + lwwg\n\
    \mbcj: mggn + stcr\n\
    \gwrz: frcj * dgvd\n\
    \fhpv: 4\n\
    \ldsr: sthg * nfpt\n\
    \rdhq: 2\n\
    \lzzc: vbbm * dpmt\n\
    \mfbb: 5\n\
    \jcjl: hpwz * dcvs\n\
    \nhqw: cgmg + dhlb\n\
    \sqtj: 2\n\
    \gdnz: zfvj * cgdv\n\
    \wmmw: pvhv + sbvv\n\
    \nqdb: gdnz * bbhs\n\
    \fgpd: fhpv * bvvq\n\
    \lldz: 12\n\
    \tlwp: vdbh + nztm\n\
    \cmhh: 7\n\
    \mswd: 2\n\
    \vwwb: rptr + gvjd\n\
    \pdbp: ztqc + rgfg\n\
    \mvdd: jljh * tpfh\n\
    \vdff: mhhr / rmjb\n\
    \nhzs: 3\n\
    \btcj: bjcr * cfzc\n\
    \nbqv: rzbt * wpmn\n\
    \nhcp: llzb * wdsb\n\
    \cmgd: 4\n\
    \hrvn: 4\n\
    \brvj: rsmh + glch\n\
    \zltj: sbbs * prff\n\
    \mgcc: 1\n\
    \fjrn: mhdc * fstn\n\
    \fqpq: 3\n\
    \lqtr: dtsp * pvms\n\
    \jsfd: mpmf * zccn\n\
    \lmvv: 9\n\
    \bmhv: hsvm - mftj\n\
    \hcbt: 5\n\
    \hsrq: lfnz - bgqs\n\
    \mgtj: 5\n\
    \zpdf: qzcv * jqnt\n\
    \dtbn: qvbd + hsbm\n\
    \dtrd: 1\n\
    \bmcs: djbc / zlmf\n\
    \chfp: wmdz + vmsv\n\
    \qpng: twlw * nzvj\n\
    \wtvj: 2\n\
    \lrqt: rghr * cdgb\n\
    \vqqv: sgql / jvgp\n\
    \ggvm: 2\n\
    \gvwt: 4\n\
    \msnt: 3\n\
    \rgjs: 3\n\
    \vlzq: 2\n\
    \jvlq: hhqg * pcvt\n\
    \bzhz: 4\n\
    \gvnj: rccn / clmq\n\
    \pgrb: zdpt * hfcj\n\
    \mhlb: 2\n\
    \nwcp: 2\n\
    \bshj: 6\n\
    \frmc: 6\n\
    \bczr: 2\n\
    \zdzp: 3\n\
    \bnqs: 10\n\
    \dcgs: fwzh * cjqr\n\
    \rrcs: lnnc * tqzl\n\
    \zcjz: tcnw - mtvn\n\
    \lhzb: 4\n\
    \rlfv: 7\n\
    \gsrf: qcnr + pbwh\n\
    \rccn: bzfm * gfzd\n\
    \qnft: 10\n\
    \jwhf: 2\n\
    \mhdc: lqsh + jbsv\n\
    \dbjm: pcnf * hnjt\n\
    \qmbd: tjwn * wdtc\n\
    \zscf: zzlt + vszj\n\
    \vjrh: gcgv + rjts\n\
    \tpcv: mwwq * cmgd\n\
    \mjmc: 5\n\
    \jlgf: qhcc + djrm\n\
    \bpmn: mpgc + hqvr\n\
    \qrcc: 1\n\
    \gjnw: 3\n\
    \jscs: 4\n\
    \zhnl: 9\n\
    \mttz: mtth * ljbd\n\
    \sctv: zlzq + bbsh\n\
    \nblm: jszp * hrzf\n\
    \jqnt: rgdv + lmlb\n\
    \msdw: 5\n\
    \qplq: lpzq + mjzd\n\
    \tjgz: wfsn * jdcj\n\
    \bgpf: 7\n\
    \bhnj: fpgd * lzsm\n\
    \gcbr: tqjg * qrcr\n\
    \hstn: 5\n\
    \twjt: 15\n\
    \jjth: 5\n\
    \jvvb: 11\n\
    \stqv: zmgh * hcbt\n\
    \hzld: wjjt + nrrp\n\
    \wdtc: fzwt + rcdq\n\
    \wfts: 2\n\
    \qhcg: 6\n\
    \czch: psrd * szmm\n\
    \tdpc: 5\n\
    \fgsl: qfqp + rsdd\n\
    \gjqv: vwqd * sprb\n\
    \ljhs: pfrp - fdts\n\
    \hbnl: 11\n\
    \qgbg: 2\n\
    \vpvz: dpmz * dsvn\n\
    \fpjm: 3\n\
    \dvtc: 2\n\
    \hvrw: rlsw * bgfl\n\
    \bvnz: 3\n\
    \jggg: 7\n\
    \czzp: vnsw - qpnz\n\
    \vnsw: dtfp * srgl\n\
    \bnbr: 7\n\
    \jnjd: stqv * gwzj\n\
    \bpsw: hcfd + vlzr\n\
    \nffn: 2\n\
    \nqsz: pdnf * cscw\n\
    \rjrh: czsq * snjh\n\
    \vvss: bnrw + pbwm\n\
    \cvrr: pvbl * mbfb\n\
    \zjnc: 3\n\
    \mfwc: srvj + dbdm\n\
    \hswd: 2\n\
    \gzll: 5\n\
    \vwwh: 7\n\
    \dlrv: lmzh + bhnj\n\
    \pjqs: hcwz + bppc\n\
    \cmgj: jqcc - bzsv\n\
    \hpfg: mjws * bmzj\n\
    \cvmz: qljr - zwgw\n\
    \dhbh: 4\n\
    \dqhd: pcdb * zjqb\n\
    \fwsc: tqhd + dmtg\n\
    \dccz: scqr + ltdc\n\
    \cjht: 1\n\
    \prsn: qccw + tmnp\n\
    \vpgd: bgvz * gbdd\n\
    \rlqv: tqvl + jgmj\n\
    \jwbb: djcs + zgcs\n\
    \qzpm: 3\n\
    \hpwz: smmw * mgtj\n\
    \qdwp: plvj * chmp\n\
    \ndlq: 5\n\
    \hmld: glcj * jvtq\n\
    \vcvg: 5\n\
    \mvrh: 2\n\
    \qrrt: 5\n\
    \thfv: 3\n\
    \csqs: 4\n\
    \rqwr: qhss + gscg\n\
    \ndsg: lssl / ffqm\n\
    \rqlg: vbjt * hntd\n\
    \wwmz: 2\n\
    \hjrs: 19\n\
    \zwhn: ppwp * mwtw\n\
    \zvqd: 2\n\
    \jdpf: ldbj + tszq\n\
    \bpnm: trcz - nbcp\n\
    \vfvp: gpmm - rlqv\n\
    \wgsm: 5\n\
    \tljl: bshj * hwlv\n\
    \trjr: mprp * gfnm\n\
    \dcvs: 5\n\
    \qlhl: 8\n\
    \ldwz: 2\n\
    \qsqp: dprl + dzcz\n\
    \dzcz: ffcb * ffcz\n\
    \bppc: 2\n\
    \wpwd: twgq * vnmn\n\
    \tstc: 5\n\
    \gvqd: jmsp / vvqm\n\
    \tdhh: 7\n\
    \llzb: 2\n\
    \rngj: dqlh * ggrc\n\
    \wbwl: fzqb * sshz\n\
    \lqzf: 1\n\
    \tvgl: 5\n\
    \dzmr: 7\n\
    \nbfb: 3\n\
    \lbgm: 4\n\
    \jdrl: htln * thvn\n\
    \fhph: wzzs - fbrg\n\
    \mhvq: 2\n\
    \spbq: 4\n\
    \pjmp: thfv + vplv\n\
    \wzjp: htbq * pwff\n\
    \qvpb: hfsf + nddc\n\
    \dtfp: 7\n\
    \twcb: 2\n\
    \jszp: wtbn / ddmh\n\
    \cmqs: 2\n\
    \htln: 7\n\
    \zjgs: 3\n\
    \zqwj: 2\n\
    \vfpp: sssl * vvzw\n\
    \jqfr: 3\n\
    \mtps: jcjl + hpwt\n\
    \pnhh: 3\n\
    \zqgm: lwpb + zrbz\n\
    \glch: gfwj + zdjv\n\
    \hhgm: 5\n\
    \fngj: 10\n\
    \mgvs: 11\n\
    \gvsp: 13\n\
    \mfpd: 4\n\
    \tmqs: 14\n\
    \fnzq: 4\n\
    \ntsg: gglw * qthn\n\
    \tnts: tpcv * zfcv\n\
    \bnrw: 6\n\
    \qhnh: rhvg + rggq\n\
    \gclw: vrjz * tdtn\n\
    \nczf: lzhz * wnjj\n\
    \bdgh: 3\n\
    \stft: 4\n\
    \hrdc: 5\n\
    \dhwv: rjmh + qjvf\n\
    \lsjh: 11\n\
    \vzcj: lhrf + qvvr\n\
    \lnrp: 3\n\
    \mhsz: 3\n\
    \dcbf: lscv * bvrp\n\
    \jqtp: 3\n\
    \vstg: 11\n\
    \trlq: 3\n\
    \wrcv: sdhs / ggcv\n\
    \nvvn: 3\n\
    \glpc: 4\n\
    \fcls: 9\n\
    \cfzc: 9\n\
    \mzfr: fjzz + zclm\n\
    \gpdt: 3\n\
    \vhvz: 3\n\
    \dfwn: jgll + zmll\n\
    \nwcs: 4\n\
    \npfj: 1\n\
    \gwzw: 2\n\
    \rhgj: 3\n\
    \fwdw: shjg * nggd\n\
    \sdvh: 2\n\
    \fhsp: 13\n\
    \nbcp: 1\n\
    \jtpb: hfzg + bdvl\n\
    \jjgz: vcmv + tgsj\n\
    \mghp: tjdw * bjgg\n\
    \cdqb: 2\n\
    \bhwg: hqrf + vstg\n\
    \hwlv: 11\n\
    \fjzz: 13\n\
    \bqzh: hzlr / mtfp\n\
    \qgmm: tcmv + wmcf\n\
    \dzcc: wmmw * qgtt\n\
    \vmsv: mtld * pfjr\n\
    \gtgs: cdrp * vfmz\n\
    \tlgr: 12\n\
    \zwdj: lzgm * nvvn\n\
    \rggq: jmjv / brmj\n\
    \tlfr: 8\n\
    \mpjj: tnrd * mggd\n\
    \hhtp: 5\n\
    \hcfj: nqbh - pngr\n\
    \ssml: ggjj + gwbd\n\
    \gscg: cjll / fdms\n\
    \hnjt: sncq + wfgr\n\
    \trtm: 2\n\
    \nshm: 1\n\
    \rpwl: znbf - mbvm\n\
    \bwqr: pfcd * vzcj\n\
    \dmtg: vqqv * cvgh\n\
    \vbrl: hrvn + fvhw\n\
    \tqss: fvht + djzl\n\
    \nfcr: rqlg * dtlz\n\
    \jnvp: wbwl + ldgj\n\
    \rvlt: 7\n\
    \dfhh: npfj + cvfn\n\
    \tcdn: nbth * pbrj\n\
    \czsq: 3\n\
    \whvg: wwtp * tmqg\n\
    \llpb: 19\n\
    \vtqf: lfcf + qgzf\n\
    \szhv: 3\n\
    \cscw: rgvw + qlvv\n\
    \jrwt: rpsg + wqst\n\
    \shjg: 2\n\
    \tqhd: fcfd + wlnf\n\
    \cfrn: 3\n\
    \wqcl: 2\n\
    \tpvf: 6\n\
    \zfcv: ppqz * zwsf\n\
    \clbj: jvgr * wbgn\n\
    \gwsj: 8\n\
    \gmtl: 7\n\
    \lmts: 2\n\
    \mmgr: 2\n\
    \zmhf: vrvz + bvnb\n\
    \sqdq: 2\n\
    \wltg: drwl / wfnj\n\
    \hwzz: 7\n\
    \chmp: wvcs * mgfc\n\
    \bhbr: ghdb * jpsq\n\
    \rrts: dtbn / mtgf\n\
    \lctc: tqzj / lmvv\n\
    \nvcl: brzp / bzhz\n\
    \zmtj: 2\n\
    \tvzn: lwtv / jnlt\n\
    \qrpf: 3\n\
    \mflw: gmgs + zdvc\n\
    \zlqs: 3\n\
    \qwsv: rtbv + scdj\n\
    \gwzr: 2\n\
    \hgnm: 2\n\
    \sbtg: mflr * tpmw\n\
    \gjsh: qnhl + vnqj\n\
    \mcqz: dvdp - fntw\n\
    \qbqz: 3\n\
    \pvms: 4\n\
    \qthn: wrdj + mmtl\n\
    \jcgg: 6\n\
    \jpfd: 4\n\
    \ghqn: tnhl + vnst\n\
    \smmw: msdw + tpfr\n\
    \tgld: 2\n\
    \jnvl: 2\n\
    \zpjh: 3\n\
    \tsjd: fmrv - pgsq\n\
    \flvp: mztm * qgbg\n\
    \pmzp: 5\n\
    \cdgz: 5\n\
    \vfnr: wrcp * zslc\n\
    \fstj: zfld * mdvn\n\
    \tgdn: tlqz * lnvr\n\
    \hznd: qljh * bzww\n\
    \vgbp: lpfw * vcls\n\
    \ftcz: 3\n\
    \jtrf: 1\n\
    \qnhl: hfql * dnsf\n\
    \jwtf: fwnr / nbpr\n\
    \ddcm: mlrh * hbfm\n\
    \qcpp: 3\n\
    \jbgh: 3\n\
    \zqwc: bnvl + rhgj\n\
    \tsws: nblm + jbgh\n\
    \gdvz: rdvh * ccbp\n\
    \lnln: 6\n\
    \clrg: nshg + cvrr\n\
    \ptqs: 2\n\
    \bhwh: fltm / vjvv\n\
    \nlfz: tzbb - dccn\n\
    \pmjb: bsrc * vmgc\n\
    \jllw: 3\n\
    \jwbp: 5\n\
    \mgrn: nvwm + qmll\n\
    \pjhm: vpfq * hfsj\n\
    \nnnt: 4\n\
    \jhmw: pdmh / mmgr\n\
    \bzqq: 4\n\
    \flmn: 4\n\
    \zccn: dcsf + jcgg\n\
    \lwqf: 6\n\
    \wmqn: bsdn * hqjq\n\
    \bnjf: 7\n\
    \trvn: 5\n\
    \sbsp: 15\n\
    \mpvn: 6\n\
    \jdcf: pzws + swvc\n\
    \tpfh: lctc + mtps\n\
    \hpjc: 2\n\
    \bnng: 3\n\
    \dswq: qmdf - nwcp\n\
    \rsdd: 7\n\
    \sdcf: mplv * qcpp\n\
    \qpcm: 5\n\
    \rdvh: tgww + rprl\n\
    \rmjb: 2\n\
    \mggd: 3\n\
    \bfvr: 1\n\
    \lqsh: 12\n\
    \tlts: gdmq * nzfp\n\
    \jnlc: zjtb * rmhv\n\
    \hhqg: 9\n\
    \tlfd: wsmp * zqwc\n\
    \mhjj: wmms - rdmr\n\
    \vbpn: tnfb + fgpd\n\
    \rdhh: 5\n\
    \gqfr: 5\n\
    \jqcc: bjjb * qncq\n\
    \fvfw: 3\n\
    \scmg: fgsl * brgr\n\
    \jthc: 4\n\
    \vjct: msth * shgn\n\
    \pvjd: 2\n\
    \zfvj: 2\n\
    \lzpl: rwls * tgld\n\
    \mbvm: 9\n\
    \cqjv: bscm * hjrf\n\
    \bnbw: 3\n\
    \qhsj: 2\n\
    \dmpw: 2\n\
    \rtzw: 4\n\
    \blwm: qwjc + vwwb\n\
    \qctz: 2\n\
    \vlzr: hzzz * wwzt\n\
    \rvhn: whmj + vbsz\n\
    \zrjv: zvhm * hqld\n\
    \fqlw: 14\n\
    \qjgw: gvqd * hrqr\n\
    \lftp: 3\n\
    \hpmm: bvnz * vfds\n\
    \wstq: nrwm / jlph\n\
    \bdjh: 19\n\
    \rfmm: 3\n\
    \vlrn: jcgw - znmp\n\
    \nctt: 5\n\
    \pjrt: 2\n\
    \hzbc: 5\n\
    \brzp: blrp + pzzc\n\
    \gqzn: 2\n\
    \hlhs: fnbj * hmcr\n\
    \wnqq: rpjg + jzwf\n\
    \pvbl: 5\n\
    \wnjj: 3\n\
    \rgsm: pjgg + rqln\n\
    \tszq: 2\n\
    \tpsr: nvbv * wgmw\n\
    \frfc: mjsn * qrrt\n\
    \zhmr: 3\n\
    \tnfb: pmjb + hbzc\n\
    \bvbc: 5\n\
    \jdff: 14\n\
    \jscf: bcbm * rvlt\n\
    \ncrn: 2\n\
    \plpq: 4\n\
    \ndpz: 3\n\
    \cgfg: qgmm - nsft\n\
    \qftb: 3\n\
    \fhdd: 15\n\
    \jsjd: 3\n\
    \lnbl: tttg + tqrw\n\
    \wdqs: 4\n\
    \tdtn: 5\n\
    \wmms: dfwn - gzll\n\
    \dgvz: tdlb * dbzr\n\
    \mlrh: hstz * nffn\n\
    \rmjj: 7\n\
    \gfcd: mfzd * wvqs\n\
    \prfv: 2\n\
    \gtjc: sdmj - wmvl\n\
    \gfzd: 5\n\
    \vvgw: 17\n\
    \bvvq: ptql * qtgf\n\
    \gcsw: lnrg + nrgr\n\
    \rlfn: 2\n\
    \dhhj: 3\n\
    \dpbj: jwfs - zhhd\n\
    \lzhz: 3\n\
    \nbqj: 2\n\
    \ppzr: 5\n\
    \djcd: 4\n\
    \nbth: 2\n\
    \qmrl: qrqj * bbts\n\
    \hlqn: 2\n\
    \hwgn: sjrp * nrrd\n\
    \rfqc: sfvv * wszw\n\
    \ggmj: 12\n\
    \jtzf: 3\n\
    \fwzh: zplg * rbpw\n\
    \jgll: mtjr * gdqz\n\
    \cvgh: 2\n\
    \mhss: 2\n\
    \rlqq: 4\n\
    \qdqb: sgzb * nnpw\n\
    \nqqn: 3\n\
    \rnlm: 17\n\
    \wqst: dbtp * gqzs\n\
    \cfwf: 2\n\
    \bgfl: 5\n\
    \zzcz: 2\n\
    \vngl: dtlt * fqbv\n\
    \jghr: 2\n\
    \cljd: mhsz * bztd\n\
    \thrv: 2\n\
    \stcr: 4\n\
    \mcvb: qmbd * jdsq\n\
    \nddl: gcsw * zpcw\n\
    \bvnb: vdjv / pmzp\n\
    \jlzl: 2\n\
    \mgfc: 13\n\
    \zhbv: ntzf + rtdl\n\
    \vsrb: ghbd * zdzp\n\
    \bwtt: 2\n\
    \bsdn: zhnl + zqgm\n\
    \sjvq: bshr * jqnh\n\
    \rptr: chzq + fdgl\n\
    \rpsg: qfwb + nlgs\n\
    \hbfm: mgng + vtpt\n\
    \nvth: 2\n\
    \stzf: 5\n\
    \thcm: 1\n\
    \cgdv: 4\n\
    \bzpc: 4\n\
    \bbrt: 5\n\
    \rdhv: dvcj + bnng\n\
    \qgtt: trft + wwzn\n\
    \vjbp: vfzp + lljm\n\
    \vbvt: 2\n\
    \hfsj: 17\n\
    \zpjm: gndf * vjsl\n\
    \mtfp: 5\n\
    \wrbp: 20\n\
    \crhq: tgcb + smvr\n\
    \qcnr: blwm - pbhf\n\
    \nrcz: 13\n\
    \hsvm: zcjs * ssrg\n\
    \pnqh: 4\n\
    \vqlc: 3\n\
    \gwdm: bgrs + cnqm\n\
    \gvrb: ldsr / lqvj\n\
    \mlwc: qqqq * ggvm\n\
    \nzfp: 3\n\
    \jcsh: 2\n\
    \lrtf: 1\n\
    \rsmr: 5\n\
    \zvhm: 2\n\
    \llrs: 2\n\
    \grgj: 3\n\
    \jrrg: jghr * tvzn\n\
    \jdmz: 3\n\
    \ghdm: hcsb + dfhh\n\
    \wmdz: bvjw * sgrq\n\
    \bpwm: 8\n\
    \gvzz: 15\n\
    \cfcp: 17\n\
    \hmcw: nwzn * wljz\n\
    \hqmq: 19\n\
    \mbfb: rqrh * nqnh\n\
    \trcz: jzvd + cslh\n\
    \zdss: 2\n\
    \twtq: 2\n\
    \rcnd: lmhv * sdvh\n\
    \bfvw: dhjl * vqnw\n\
    \glsf: drjb + jzzq\n\
    \lgsn: tmtd * zjgs\n\
    \vdrd: 12\n\
    \gjfr: gcgp * pngb\n\
    \qhgd: pmrj / hdbv\n\
    \gwbd: gsrf / hpjc\n\
    \wffq: 6\n\
    \lmzh: jmvf + rdhq\n\
    \qlvv: plpq * mrmb\n\
    \tqjg: 2\n\
    \jdsq: 9\n\
    \fbgh: 12\n\
    \trzv: llpz * lbwn\n\
    \tncb: 2\n\
    \qmdf: 9\n\
    \cmnd: 1\n\
    \hrcl: 3\n\
    \tbsz: wmbz + zqwj\n\
    \nsqz: vgdq * hcfp\n\
    \vbwf: fwsc + dgvz\n\
    \qrcr: htrq + fngj\n\
    \rqln: tqpm * trlq\n\
    \psvp: jtrf + sbjl\n\
    \zmgh: sqsb + bnld\n\
    \zgvb: 3\n\
    \nszb: stth - lqsg\n\
    \sshz: rftq + mhvq\n\
    \wwbb: srwb + nfcr\n\
    \lsms: 17\n\
    \srjl: 3\n\
    \vzzl: 5\n\
    \zwjm: 3\n\
    \wshc: qdhd * dddt\n\
    \jzhl: 4\n\
    \hqvr: jmgv / lwtw\n\
    \jmvf: 5\n\
    \fwnr: pdlc + lbqw\n\
    \zmtf: nzfm * clgt\n\
    \frwd: dlwm + gjtw\n\
    \lqnz: 6\n\
    \hpzg: 2\n\
    \dwgs: 5\n\
    \rwwz: hgpn + rpfb\n\
    \qqlz: lfsz * lsds\n\
    \spbs: gqzn * vhsw\n\
    \rrjv: fhpd * jzql\n\
    \qgdh: 3\n\
    \bzzw: wlfc * zmtj\n\
    \hqjq: 2\n\
    \gcgv: bpnm * vcpw\n\
    \mwrp: dpjw * bvtf\n\
    \vjvq: mppt + qgdh\n\
    \qzhz: 3\n\
    \brjh: 4\n\
    \pdnn: 13\n\
    \tpsq: sbsg * jjjr\n\
    \nlfb: 2\n\
    \dbps: scmg * pgwl\n\
    \fgmr: vfrw * fvqc\n\
    \bvjw: 5\n\
    \hwbd: 14\n\
    \wwzl: 1\n\
    \nnzl: 2\n\
    \gjtw: fhdd * cvsl\n\
    \tqbh: 7\n\
    \bshr: 7\n\
    \tflh: 1\n\
    \lnst: vlzq * vvjl\n\
    \hzlr: qpzb - wcqf\n\
    \bjjb: 2\n\
    \qvql: qwrj * ngbz\n\
    \dsgd: 4\n\
    \fcmm: hqmq + lnzc\n\
    \wfsn: fnnd * mswd\n\
    \bnwq: vdhc * chtj\n\
    \fsvs: 3\n\
    \tnhl: cppz + bfvr\n\
    \lntb: zlnl - wstq\n\
    \gdzb: wrbp + svhm\n\
    \vnst: qwbp + zggm\n\
    \wmzl: qvvg * whqg\n\
    \tdln: 3\n\
    \frcj: tzsj / ccwb\n\
    \mhwg: cfrn * srjl\n\
    \gqtm: 2\n\
    \cjnz: zhwl + wlbz\n\
    \fszt: 4\n\
    \bnld: 1\n\
    \ffcb: 2\n\
    \wwnb: 3\n\
    \smfb: 2\n\
    \tqpm: plhh * wbdq\n\
    \hmcr: 3\n\
    \fmmh: tqmn / sqtj\n\
    \gvjd: bhcs + fbqr\n\
    \mftw: dllq + svrs\n\
    \mwpd: lzvq / wbwz\n\
    \rnvj: 5\n\
    \fgdr: 7\n\
    \zcvz: zwbb + qzmv\n\
    \zgpj: 1\n\
    \lrnm: 3\n\
    \hmmm: mlwg * zbbp\n\
    \lqbg: jsth * jnvl\n\
    \rgwm: 3\n\
    \zfmp: 4\n\
    \dfdq: 2\n\
    \wcwc: wgfj + lpdz\n\
    \dppg: cntp * gbmj\n\
    \shgn: 2\n\
    \qfqp: 2\n\
    \fcwd: 14\n\
    \jznp: 8\n\
    \nzfm: 5\n\
    \zzsg: 2\n\
    \zscw: 5\n\
    \rsjs: 5\n\
    \hwrw: 2\n\
    \wjrp: 2\n\
    \tdvz: hwqw - tgdn\n\
    \nchh: 1\n\
    \dhjl: vmcz + dnzj\n\
    \pmpp: gssj * jjgz\n\
    \vhnp: 1\n\
    \jsth: hrcl + tdrr\n\
    \rhsn: zphd * gpdt\n\
    \cmts: wzwt + qmqb\n\
    \nffw: qpwf - fgdm\n\
    \dtmz: 5\n\
    \jmsp: qhnb / qlhl\n\
    \vnrb: 2\n\
    \cjcw: 5\n\
    \qpjb: gcgz * trzv\n\
    \tghf: 9\n\
    \jjgg: pwpz * wrdb\n\
    \mjws: gfbb + ngsz\n\
    \zslc: llpb * lrlz\n\
    \fbwp: ccwf * psrm\n\
    \ndjr: hqhr + gbzw\n\
    \nrwm: ctzp + vdgn\n\
    \nsft: rwbj * gwfz\n\
    \tmmq: 5\n\
    \mrjg: 2\n\
    \shjz: 3\n\
    \lqsg: 5\n\
    \hmhl: 2\n\
    \wfnj: 2\n\
    \pzhr: 4\n\
    \drjj: cfqj - fwzj\n\
    \rjft: 4\n\
    \wszw: 2\n\
    \rghr: 13\n\
    \vpgq: rvcc + wdcl\n\
    \ffdz: 2\n\
    \root: qmfl + qdpj\n\
    \gmrc: 2\n\
    \sstb: 2\n\
    \vpnv: 3\n\
    \bgqr: sspn * lrph\n\
    \vtqm: 2\n\
    \qrrv: 4\n\
    \tqzh: tczc * zpmd\n\
    \mprp: gwsj + clrg\n\
    \wfsd: 4\n\
    \pjgg: 1\n\
    \llpz: tjgz + vhnp\n\
    \bqml: rhqh * dvzw\n\
    \bgcf: 3\n\
    \rdnt: 5\n\
    \sjrp: 6\n\
    \wvqs: 5\n\
    \jvgr: ndlq + gjqv\n\
    \mtld: wlmt + rrts\n\
    \tpfr: 14\n\
    \bjhl: 2\n\
    \jhqj: mgzg + ghbg\n\
    \jrdn: wdwc * nszb\n\
    \dncr: 2\n\
    \srwb: twpb * cnmp\n\
    \llms: 12\n\
    \dwhw: gflz * zzsq\n\
    \mlpm: ntsg + vpvh\n\
    \rddc: rnrn / tncb\n\
    \wlnz: gbcm * drgg\n\
    \gsfv: zrjv / zzsg\n\
    \lfsz: zmtf + mgcc\n\
    \qbbp: 7\n\
    \qgnv: zgfw * wnsl\n\
    \fnpt: psjz - tghf\n\
    \ntcj: 3\n\
    \jmgv: fvts + rgsm\n\
    \rbsz: 3\n\
    \rnhm: 3\n\
    \gclh: jqtp + fjgs\n\
    \gpbs: 5\n\
    \cdgb: nbqv + pdnn\n\
    \vgrw: 1\n\
    \crdz: 1\n\
    \vmgc: 2\n\
    \pbwm: 5\n\
    \fwnb: 3\n\
    \fmrc: bqcr - ffmz\n\
    \pzzc: dhhj * vpnr\n\
    \wlnf: 8\n\
    \smnm: nvfl * jntg\n\
    \wlfc: cllv - bbjg\n\
    \ngfl: mvdd - ntvr\n\
    \ftwh: 5\n\
    \djtq: prfv * mgzq\n\
    \bvzd: 2\n\
    \hbmn: tjlg * mqcr\n\
    \lvgb: 3\n\
    \jvbn: fvnh + vfnr\n\
    \wdsb: dcgs + sdvv\n\
    \vrbl: 8\n\
    \rpjg: ssml * dcbf\n\
    \phpl: 1\n\
    \ggjw: ngzt * jmzz\n\
    \qbpc: 2\n\
    \qfpw: wjrp * msqp\n\
    \qrqj: 3\n\
    \nvlg: sgnr * rlfn\n\
    \pvhv: jpfd * zgsd\n\
    \wcql: vbpn * gfqw\n\
    \tpvg: 14\n\
    \tqvl: qjnb + szmh\n\
    \stwj: 14\n\
    \qpbw: sqwl * cwsh\n\
    \hsgz: 3\n\
    \twpb: 3\n\
    \lzqp: qrpf * jdmz\n\
    \cgtt: dmjh + nqsz\n\
    \zgrl: 1\n\
    \lttn: 9\n\
    \pfcd: 5\n\
    \rbds: 8\n\
    \ffmz: 5\n\
    \qdhs: 17\n\
    \pcnf: 3\n\
    \wnmd: 2\n\
    \nwvb: dtsm + bgcs\n\
    \wzzs: mvbb + bstc\n\
    \flvl: sbdv * wwmz\n\
    \zgfw: ctth * vdms\n\
    \dvpj: wftp * rljr\n\
    \mgzg: 5\n\
    \zzsq: 4\n\
    \bwwh: 5\n\
    \hpwt: frfc + bpfl\n\
    \hdtt: 3\n\
    \plhj: bhhr + zldq\n\
    \zlzq: rrjs * bbwr\n\
    \sgrq: 11\n\
    \dsvj: dplh + mcvb\n\
    \lzgd: bjdz + tgbb\n\
    \sbjl: 10\n\
    \dvhs: 1\n\
    \wqqh: 7\n\
    \qzmv: 5\n\
    \csrl: 19\n\
    \glzv: nhvb + ttjz\n\
    \bdht: 18\n\
    \pwpz: llsn * jlgf\n\
    \zwzw: mnwt * sbsp\n\
    \rjtn: gbsg * vznp\n\
    \zzrs: 10\n\
    \wzvv: djtq + rmdp\n\
    \sgnr: glzv - tcdf\n\
    \pmbz: mhjj + tdvz\n\
    \dtsm: 11\n\
    \bbjg: 3\n\
    \gnzs: whvg + stlh\n\
    \ljjw: dwgs + fqpq\n\
    \rnrn: nvlg * gtmh\n\
    \swnv: htdh * rwzw\n\
    \sgzb: 2\n\
    \tctg: 3\n\
    \wqqr: 2\n\
    \cslh: vbvt + hrdc\n\
    \dsgc: 8\n\
    \qvbn: 2\n\
    \gmgl: 8\n\
    \sqzg: gjch * tsmq\n\
    \qjnd: 3\n\
    \lnpd: mllp * czch\n\
    \tqgq: fmrc * hcfj\n\
    \rfjb: 6\n\
    \hbnc: swzv * jrrd\n\
    \zmjq: wpjf + zrjr\n\
    \nbpr: cdqb + srzw\n\
    \zzlt: jdpf * dccz\n\
    \rgvw: 5\n\
    \jtlm: dvmd + mhlb\n\
    \wljz: 2\n\
    \ngnz: wfts * flnj\n\
    \lwtw: 2\n\
    \gcgp: lvdv * rffj\n\
    \zfld: tdwg / hbsl\n\
    \ntcf: bjfr + bffs\n\
    \rnfj: 8\n\
    \cjzd: fmmd * ggjw\n\
    \fljw: 2\n\
    \wlsq: dtfb * qbqz\n\
    \hwqw: wdqh * jfdb\n\
    \slpg: 7\n\
    \htvt: 3\n\
    \gfqw: 3\n\
    \cwrn: chfp / hltj\n\
    \sfpv: pzzf + qbbp\n\
    \zsrb: 2\n\
    \bbwr: mzwc * wmjw\n\
    \jrzr: 3\n\
    \chtd: 18\n\
    \jvfw: rdmc - dvlz\n\
    \swvc: 5\n\
    \lcgn: bdgd / snvf\n\
    \jmzz: 2\n\
    \bstc: 3\n\
    \qbhv: sdcf + hhdn\n\
    \ldgj: qrrz * bpmn\n\
    \fnwz: 3\n\
    \rjts: 18\n\
    \fbqr: fbbz * jdrl\n\
    \dvlz: 2\n\
    \mvbb: 19\n\
    \jbwj: 2\n\
    \vcpw: 2\n\
    \lcjw: 3\n\
    \ljbc: 2\n\
    \gslz: tgtb / fnpz\n\
    \bwrp: 5\n\
    \gslt: 15\n\
    \mjzd: vhnl + jdff\n\
    \fslb: 1\n\
    \tddz: rnfj + rhsn\n\
    \smfh: 2\n\
    \ljhw: jchg + gpnj\n\
    \ggrc: 3\n\
    \njjd: cfcp + wnvb\n\
    \jpsq: 4\n\
    \fvnh: vthj / pbgp\n\
    \hjrf: 3\n\
    \dhpq: 3\n\
    \hlpd: 3\n\
    \jnmz: 15\n\
    \trdn: vpgq / zvnw\n\
    \tjlg: njhv + dgbh\n\
    \pgmp: 2\n\
    \lrvc: dhwv + prtq\n\
    \vvjl: 3\n\
    \qzcv: cjzd - bnwq\n\
    \hntd: 3\n\
    \ptcq: bmqc + cjnz\n\
    \cqjp: 9\n\
    \czsr: 2\n\
    \mrmb: 2\n\
    \jbsv: nbqj * tstc\n\
    \dlmb: 3\n\
    \qtlb: 3\n\
    \lnzc: 7\n\
    \hcwp: 3\n\
    \wcnl: 7\n\
    \rtbv: cqpd - lftp\n\
    \tqzl: 4\n\
    \jgnl: 2\n\
    \dgsq: jsrr - wwnb\n\
    \jcls: 4\n\
    \vznp: 6\n\
    \stlh: mgvs * tcdn\n\
    \nptd: 3\n\
    \wrzd: thtw + zwlf\n\
    \mfvg: 3\n\
    \pgsq: 4\n\
    \bpfl: swlr * qttr\n\
    \rgfg: 9\n\
    \jmtd: 2\n\
    \sbdv: 12\n\
    \rjmh: 18\n\
    \rhvg: 5\n\
    \dddt: 2\n\
    \qjnb: 1\n\
    \ffcz: wfrm - wfpv\n\
    \lqpd: qstp * mrjg\n\
    \wfpv: czbq * vdcg\n\
    \ppmj: wwbb * frmc\n\
    \zwbt: mqzg + dgjc\n\
    \qwrj: 4\n\
    \gwfz: fvhf * qpgs\n\
    \tfht: phzl + ptpg\n\
    \nwdd: mwpd * qnjm\n\
    \whqg: 2\n\
    \lqgv: nnpn * lnbl\n\
    \fwzj: 2\n\
    \trft: 2\n\
    \hbmg: 7\n\
    \lnnc: lrnm + jnch\n\
    \tmtd: 5\n\
    \nrwb: 3\n\
    \bgqs: 4\n\
    \cvdg: bbrt * jrbl\n\
    \hfsq: 3\n\
    \whnc: stwj * rdwq\n\
    \thvn: wlsq / fwdw\n\
    \ncvt: qfgs * qjgw\n\
    \mccs: vvwr * lzqp\n\
    \mtjr: 3\n\
    \wpmj: glpc + hhqs\n\
    \svfp: 4\n\
    \gglw: 2\n\
    \qdlt: 2\n\
    \rvcc: nffw * zsrb\n\
    \vbjt: 7\n\
    \mnwt: 3\n\
    \fwpb: 7\n\
    \zpcw: 3\n\
    \gpjj: 2\n\
    \zlnl: zvqm * jjgg\n\
    \grzq: 5\n\
    \fbmh: 18\n\
    \nztm: 18\n\
    \bdvl: 5\n\
    \mzgc: mccs * pnbf\n\
    \zmll: wvzt * hlvb\n\
    \jzwf: sqzg * mnqr\n\
    \fsld: mvzb * rpjn\n\
    \vfdn: 5\n\
    \mbfj: dvwj + mftw\n\
    \zwlf: hnhg * cnhn\n\
    \qljr: nhgm / sqdq\n\
    \nmbh: nhqw / vdgc\n\
    \pszt: 14\n\
    \vjvv: 5\n\
    \psjz: tddz + fwqf\n\
    \dllq: 2\n\
    \wftp: 3\n\
    \bzss: ccsj * fpjm\n\
    \blrp: cmgj * pgmp\n\
    \dqmq: 4\n\
    \rcmt: cwtl * shqt\n\
    \rfmb: 8\n\
    \gqhj: mdtt / rsmr\n\
    \mrzl: vgql + dpzn\n\
    \lmdt: 19\n\
    \vvbm: svrc + cfwf\n\
    \dnsf: 5\n\
    \gnct: 5\n\
    \ccgr: glgp * jfrl\n\
    \jcgw: 14\n\
    \mvzb: 2\n\
    \qpnz: 4\n\
    \hfzg: lqzf + fhrp\n\
    \cnrt: 2\n\
    \pbcq: sncj - fslb\n\
    \htdh: 2\n\
    \hbsl: 3\n\
    \zpqh: 8\n\
    \vhvn: fsld + qvql\n\
    \dzjf: 15\n\
    \jvnj: gzqv * mbcj\n\
    \jhnc: 2\n\
    \sngg: 2\n\
    \rmdp: lzzc * jbwj\n\
    \rhll: dbgp + nzzr\n\
    \svrc: 5\n\
    \bscm: 3\n\
    \vcls: 2\n\
    \cnqm: 8\n\
    \nnfj: clbz * wdqs\n\
    \sdmj: bvzd * hsrq\n\
    \vcmv: ngfl / qpcn\n\
    \bmzj: 7\n\
    \fgqh: 2\n\
    \mzcv: dpwl * blcn\n\
    \smqn: 3\n\
    \zhwl: cgrn + qwsr\n\
    \pppb: 10\n\
    \dlwj: cgvz * btsd\n\
    \cfqj: hfnp + hswd\n\
    \bgrs: 5\n\
    \gndf: 3\n\
    \wvcs: 2\n\
    \rtlc: jwtf + tbzv\n\
    \cnmz: hmcw * rbbm\n\
    \ltmj: 3\n\
    \gbdn: thvd - pbdd\n\
    \ttjz: 16\n\
    \mmtl: rcmt + cgfg\n\
    \mbfg: thgv * brvj\n\
    \clmt: nlrm + lhfm\n\
    \vnmn: vjrh + nddl\n\
    \cwjz: rjrh + pgrb\n\
    \btsd: 9\n\
    \pbgp: 4\n\
    \jzql: 3\n\
    \ntvr: qgnv / vcvg\n\
    \pcvt: jnfj * bcmq\n\
    \lcjl: wcvg + svwc\n\
    \czbq: 10\n\
    \gbzw: qqds * whpf\n\
    \hvgh: qmfc + qjzs\n\
    \tmnh: rnvj * cmqs\n\
    \sncj: jcqf * sldq\n\
    \hqfz: 5\n\
    \ftrz: 4\n\
    \qgzf: 5\n\
    \lvvf: 3\n\
    \ghnh: fvfw * fbmh\n\
    \vvqm: 2\n\
    \nqzq: sbtg - cwrn\n\
    \rzbt: dctc * nlfb\n\
    \ggzw: zmhf + pjhm\n\
    \wfrm: vhvn / zdss\n\
    \fdms: 3\n\
    \lgtr: 2\n\
    \ngsz: 2\n\
    \cbls: 3\n\
    \fdgl: gcvd + vvgw\n\
    \hplz: jvnj + hhnd\n\
    \wzwt: zwdj + ndls\n\
    \jzvd: 5\n\
    \tntn: 11\n\
    \cfpv: tgdw + lcjw\n\
    \ljbd: fcls + plnd\n\
    \vgnt: 5\n\
    \pqjl: hjrs * tdhh\n\
    \wtsp: qhsj * vzbp\n\
    \tcjn: jpnn + vgnt\n\
    \vdgc: 3\n\
    \drwm: 4\n\
    \zmzs: jvvb * jlzl\n\
    \bcbm: 2\n\
    \jhgg: njjd - qpgj\n\
    \prtq: pnqh * pbht\n\
    \gpnj: mpvn * jhnc\n\
    \cbbv: 2\n\
    \fpnq: 13\n\
    \zvch: hwzz + lzgd\n\
    \qgtc: hbnl + hfst\n\
    \djrm: ftrz * vfdn\n\
    \hhdn: 5\n\
    \fntw: 10\n\
    \nmhw: 5\n\
    \dgnb: 2\n\
    \zdjv: 6\n\
    \rmcj: qpcm + pmtv\n\
    \lrlz: 2\n\
    \rgdv: czsr * zfmp\n\
    \qhcc: ghrc + brcc\n\
    \lscv: 14\n\
    \hmgq: 3\n\
    \rjgc: 2\n\
    \wmqt: wrcv + tgrs\n\
    \dtlz: 2\n\
    \qnjm: 2\n\
    \tjzz: gtgs * fpnq\n\
    \qdhd: clfq + fqtl\n\
    \plbn: ghzz + gzhl\n\
    \dctc: qqtc + lrtf\n\
    \fltm: zmmp * jrwt\n\
    \lssl: jcsh * ntqg\n\
    \djzl: wffq * lbhn\n\
    \vmcz: 6\n\
    \jtrl: 5\n\
    \zphd: 2\n\
    \hqrf: 19\n\
    \wmzq: 4\n\
    \thtw: zltj * phqm\n\
    \hgpn: 3\n\
    \wlbz: dsqs + csrl\n\
    \zjtb: 3\n\
    \rwsc: 9\n\
    \vdqf: 7\n\
    \njht: wcdg + grzq\n\
    \mgng: fszt * zzcz\n\
    \pgwf: 2\n\
    \twgq: 9\n\
    \lbwn: 3\n\
    \qmll: 7\n\
    \jlph: 2\n\
    \jchg: 7\n\
    \wsmp: 16\n\
    \humn: 335\n\
    \lsds: 9\n\
    \msgc: 13\n\
    \wpmn: 2\n\
    \ggjj: tngv * rhww\n\
    \rftq: dnvh * nhlz\n\
    \nzvj: 2\n\
    \nnpn: 2\n\
    \zbbp: cqjv - bndb\n\
    \dpzn: dppg * bgqr\n\
    \zbtl: 4\n\
    \vdbh: smqn * lbqn\n\
    \twlw: 5\n\
    \qncq: hszg - nhvj\n\
    \brjv: ndsg + dtbb\n\
    \ghzz: tlfd + ndpr\n\
    \mwvl: 3\n\
    \qnbg: 8\n\
    \vlpn: gqtm * whnc\n\
    \zclm: nprq * rbds\n\
    \svrs: 4\n\
    \wqsz: tbsz * wqqr\n\
    \wgfj: 3\n\
    \vgdp: msjw + stft\n\
    \hnds: 11\n\
    \bdgd: wrzd * cnvp\n\
    \nzhm: pmbz / lnzw\n\
    \fhqz: 2\n\
    \glgp: 2\n\
    \fcbn: 5\n\
    \qwjc: crhq * sbhv\n\
    \lwtv: jpzh - frwd\n\
    \vzwb: 8\n\
    \msqp: mttz + qqpz\n\
    \lhmv: 5\n\
    \zdnf: 1\n\
    \vfnc: 2\n\
    \ppqz: 2\n\
    \hdbv: 2\n\
    \dpjw: rjgc + qgff\n\
    \rpzz: 2\n\
    \gwzz: 4\n\
    \grrz: jhqj * hsqh\n\
    \nfpt: bzss + pnjg\n\
    \vfcv: lmfg * phhf\n\
    \lbqn: 19\n\
    \hfst: fvzt + rpnz\n\
    \fmmd: 2\n\
    \cdrp: 5\n\
    \jpzh: hhbc * zpqh\n\
    \chrc: fwpb + rtcn\n\
    \nflm: zhsf * bwtt\n\
    \zrhr: cjfh + strj\n\
    \tdtz: jvbn * cnlm\n\
    \vdhc: 3\n\
    \ptpg: fnwz * nqqn\n\
    \vdqt: 4\n\
    \mqcr: dqmq + njht\n\
    \wmvl: 6\n\
    \wqpq: mlgg + pmqb\n\
    \pbvg: 3\n\
    \mztm: 4\n\
    \tczc: 11\n\
    \trmr: qvpb * rzcv\n\
    \msth: vgrw + tjzz\n\
    \cztz: 5\n\
    \fmzr: gzpd + rmgd\n\
    \vqnw: nshm + jscf\n\
    \hghq: zgrl + nmhz\n\
    \flcm: 5\n\
    \qfmj: jmtd * fgdr\n\
    \zplg: 2\n\
    \lwns: mzcv + dbps\n\
    \clmq: 5\n\
    \stwf: wmqn / fgqh\n\
    \hcfp: 2\n\
    \dqlh: 17\n\
    \dmjh: drwm * mbfg\n\
    \jrbl: tczz - dvpj\n\
    \vdgn: nlbv * bhbr\n\
    \fnbj: 2\n\
    \lbft: 3\n\
    \qvvr: lvcf + jnmz\n\
    \fdts: 18\n\
    \bhqn: jfvz * gvsp\n\
    \snvf: 2\n\
    \jrmz: 17\n\
    \tgrs: mzgc + pdbp\n\
    \gfwz: 3\n\
    \dvdp: ccwj * zglm\n\
    \lfcf: 6\n\
    \bznt: wqpq + gdvz\n\
    \zqnm: jsfd + smgj\n\
    \gbmj: 5\n\
    \rgpl: 9\n\
    \snds: 5\n\
    \jzbv: 2\n\
    \pbwh: cljd + spls\n\
    \jvtq: 3\n\
    \zgsq: cdlf + lqbg\n\
    \rwls: 11\n\
    \gssj: gcbr - ctjs\n\
    \phzl: vtqm * dzmr\n\
    \dnzj: 17\n\
    \spls: vdqt * lcjl\n\
    \wrdj: hmld + fsll\n\
    \gbsg: 3\n\
    \wzqw: dvrs * zlqs\n\
    \hncc: 5\n\
    \mpmf: 7\n\
    \msjw: 3\n\
    \mggn: pgfz * tcjr\n\
    \jzsl: 13\n\
    \hpgz: 16\n\
    \pngb: 2\n\
    \jqfw: 5\n\
    \cgrn: hvrw - hggd\n\
    \tcmv: tqgq + vfnh\n\
    \tzgl: 5\n\
    \btbt: 2\n\
    \sbzb: 13\n\
    \npcq: 12\n\
    \gpmm: wttd + jpjh\n\
    \wcqf: cgtt / vwzb\n\
    \mhnw: tmnh - qrcc\n\
    \rrjs: 2\n\
    \mftj: 4\n\
    \wjqq: mbjq * vjbp\n\
    \pvcc: 2\n\
    \zgsd: 4\n\
    \llgf: bbpv + vfqd\n\
    \bffs: 12\n\
    \ntzf: pjmp - wpmj\n\
    \cjqr: vcfg + nwnt\n\
    \ccff: 4\n\
    \vfrw: 4\n\
    \nlgs: jmcc + ltnn\n\
    \htbq: blfn * llrs\n\
    \psrm: llhd / wtsp\n\
    \cnvp: 2\n\
    \ssfd: 4\n\
    \szmh: 6\n\
    \zjsm: vpnv * wltg\n\
    \lbhn: 2\n\
    \wdcl: bczr * rhhc\n\
    \tscr: mlwc - sfmq\n\
    \hzlb: 3\n\
    \dttr: 11\n\
    \hhwj: 3\n\
    \bbhs: 2\n\
    \mtth: tltd + ddfr\n\
    \qdpj: wnqq * lcgn\n\
    \nhvj: sfrw * hzbc\n\
    \hfql: 2\n\
    \mbpn: cwlg * vcpv\n\
    \tczz: fhqz * cjns\n\
    \fvzt: 5\n\
    \fzdp: 3\n\
    \zwsf: 16\n\
    \ncnl: rrjv * tqss\n\
    \rrbj: 4\n\
    \gqzs: 5\n\
    \srzw: 5\n\
    \tvvc: hsgz * clzb\n\
    \jvrb: mfbb * zjqc\n\
    \jtgr: 10\n\
    \hpnp: qdwp / bvbc\n\
    \wcjn: crdz + cfpv\n\
    \rbbm: 3\n\
    \wmcf: bvwd * jgfg\n\
    \stth: hmmm * pgwf\n\
    \npnb: flvl - bnbr\n\
    \vwsn: 4\n\
    \pqmw: ffhl - tmqs\n\
    \pdgc: wtvj * vhgt\n\
    \hcwz: 5\n\
    \tdwg: cmhh * ndpz\n\
    \hgmm: ptqs * fbgh\n\
    \lmlb: 15\n\
    \qgff: 5\n\
    \dgfm: wjfb - trjr\n\
    \tnrd: 2\n\
    \hptb: 2\n\
    \lhrf: tncj * jtpb\n\
    \znmp: 1\n\
    \wvgb: 11\n\
    \nlrm: 2\n\
    \lbqw: jvlq + jmqt\n\
    \ggtd: 6\n\
    \tngv: pjgq + mbfj\n\
    \fvts: nptd * mtwg\n\
    \rljr: 3\n\
    \lqsz: 2\n\
    \gfnm: hgsr * psvp\n\
    \pzws: 2\n\
    \hvwz: 2\n\
    \jgvq: thrv * rpwl\n\
    \zfpm: hbnc * cnrt\n\
    \cvfn: smfb + nczf\n\
    \wcvg: mptg / nfqw\n\
    \cnhn: vbms * gslz\n\
    \pgfz: 5\n\
    \bsrc: bwrp + hlhs\n\
    \hpbz: 8\n\
    \lwwg: cjzw * mfpd\n\
    \tfgl: ltmj * mjmj\n\
    \jrfg: 11\n\
    \ccsj: dswq + vdrd\n\
    \ztqc: hpgz * ssfd\n\
    \qljh: 2\n\
    \fznv: 4\n\
    \jrrd: 5\n\
    \szhg: bnjf * cwjz\n\
    \hfcj: 2\n\
    \ctth: 5\n\
    \pmrj: cqrm / lfmd\n\
    \vmjm: 2\n\
    \jnlt: 3\n\
    \sfrw: rgpl + ztbc\n\
    \jmwv: zjsm + pdgc\n\
    \vhnl: 5\n\
    \qdmr: fbwp / lztn\n\
    \ddqs: prsn + bznt\n\
    \zdvc: llpf * lntb\n\
    \vrnh: sfgc * dwdd\n\
    \dbtc: 3\n\
    \vbwn: 3\n\
    \whmj: hplz + lqtr\n\
    \mfzd: 5\n\
    \lzsm: 5\n\
    \jrvm: jzsl * hvwz\n\
    \fqtl: ncrn * zhbv\n\
    \psct: rfjb * gqfr\n\
    \hbzm: 12\n\
    \lmhv: gfmp - gwrz\n\
    \gzhl: dgww * lrvc\n\
    \phgn: 7\n\
    \pnjg: bfvw + mcqr\n\
    \cpsd: hbzm - bzqq\n\
    \wfgr: pswm + qmfm\n\
    \jqnh: ctwv + hwrv\n\
    \wrcp: lvvn - npnb\n\
    \lnzw: 3\n\
    \qwsr: cpsd * nwcs\n\
    \fzwt: 7\n\
    \scpp: 2\n\
    \lqsw: humn + wpwd\n\
    \wpjf: jrmz * tspj\n\
    \zpmr: 5\n\
    \lpqw: 4\n\
    \pngr: 1\n\
    \zwbb: 5\n\
    \bjdz: jvdc - dsgc\n\
    \fvht: 11\n\
    \lpfw: 4\n\
    \qhnb: jcqj * spbq\n\
    \rpnz: bnqs + qjnd\n\
    \rftg: tdtz * ntcb\n\
    \mflr: qftb * grgj\n\
    \ggcv: 2\n\
    \lztn: 2\n\
    \ztdl: 2\n\
    \bvrp: 2\n\
    \qpgj: hmhl * fnzq\n\
    \ctjs: 5\n\
    \qqtc: jrzr * lgtr\n\
    \mbjq: tlwp + tpsq\n\
    \nshg: tflh + dqhd\n\
    \phqm: mlpm - cvzv\n\
    \nrtw: 20\n\
    \pbzj: bgpf + npvh\n\
    \pbdd: qvpm * hnmz\n\
    \nhvt: cmts + mcqz\n\
    \bmcv: dbsv * jhmw\n\
    \qvpm: qrrv + gclw\n\
    \dplh: dgsq * lrmd\n\
    \ntqg: jwbb * dbmv\n\
    \lpzq: nzhm * djzq\n\
    \rbpw: 4\n\
    \hlsq: hhgm * jcfl\n\
    \rhqh: 7\n\
    \tttg: ntcf * msnt\n\
    \rpfb: 8\n\
    \jsrr: cbls * nhzs\n\
    \dpmt: 5\n\
    \qmfc: lqgv / cmdd\n\
    \mtvn: 1\n\
    \lnvr: fzdp * jllw\n\
    \pfjr: 5"