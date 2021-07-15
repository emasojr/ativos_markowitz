######## CARREGANDO PACOTES ######
library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)
library(dplyr)
library(Benchmarking)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(xts)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(TTR)
library(nloptr)
library(psych)
library(corrplot)


# DEFININDO PARÂMETROS
W =5 #Anos para análise
k= 10#Quantidade de Ações
neg = 10000 #Volume de Papeis negociados
cut_rk=0.05 #Corte do Risco
cut_rr=0.05 #Corte do Retorno
pm = 4000 #Preço Máximoda Ação
wm = 0 #peso mínimo por ativo
WA= 365*W #Em número de dias

######## FUNÇÕES ##########
sdp=function(x){
  return(as.vector(sqrt(t(as.matrix(x)) %*% covMat %*% as.matrix(x))*sqrt(252)))
}
rtp=function(x){
  return(1/as.vector((1+(x %*% mean_return))^252-1))
}
shp=function(x){
  return(1/((1/sdp(x))/rtp(x)))
}
ig=function(x){
  return(sum(x)-1)
}
fn<-function(X){
  return(as.vector(abs(Ka-t(as.matrix(p))%*%as.matrix(round(X,digits = 0)))))
}
heq<-function(X){
  s=(((p*round(X,digits = 0))/as.vector(Ka)-w)/w)*100
  s[is.nan(s)]=0
  s[is.infinite(s)]=100
  s=s*(1+abs(1-sum((p*round(X,digits = 0))/as.vector(Ka)))*10)
  return(sd(s))
}
IBB=function(REL,cut_rr,cut_rk,k){
  D1=REL%>%filter(Retorno>=quantile(REL$Retorno,cut_rr))
  D2=REL%>%filter(Risco<=quantile(REL$Risco,1-cut_rk))
  DEADATA=intersect(D1,D2)
  DEA=dea(DEADATA$Risco,data.frame(DEADATA$Retorno,DEADATA$SHP), RTS="crs",ORIENTATION = "graph")
  DEADATA$Efc=DEA$eff
  Dados<-DEADATA%>%filter(Efc>=quantile(DEADATA$Efc,(1-k/dim(DEADATA)[1])))
  
  return(Dados)
}

######## ESCOLHENDO O PORTFÓLIO ######

# COLETANDO E ORGANIZANDO DADOS
ibov<-c("AALL34.SA","AALR3.SA","AAPL34.SA","ABBV34.SA","ABCB4.SA","ABCP11.SA","ABEV3.SA","ABTT34.SA","AFLT3.SA","AGRO3.SA","AHEB3.SA","AIGB34.SA","ALPA3.SA","ALPA4.SA","ALSO3.SA","ALUP11.SA","ALUP3.SA","ALUP4.SA","ALZR11.SA","AMAR3.SA","AMGN34.SA","AMZO34.SA","ANIM3.SA","APER3.SA","ARMT34.SA","ARZZ3.SA","ATMP3.SA","ATOM3.SA","ATTB34.SA","AXPB34.SA","AZEV3.SA","AZEV4.SA","AZUL4.SA","B3SA3.SA","BAHI3.SA","BALM3.SA","BALM4.SA","BAUH4.SA","BAZA3.SA","BBAS3.SA","BBDC3.SA","BBDC4.SA","BBPO11.SA","BBRC11.SA","BBRK3.SA","BBSD11.SA","BBSE3.SA","BCIA11.SA","BCRI11.SA","BDLL3.SA","BDLL4.SA","BEEF3.SA","BEES3.SA","BEES4.SA","BERK34.SA","BGIP3.SA","BGIP4.SA","BIDI4.SA","BIIB34.SA","BIOM3.SA","BKBR3.SA","BLAK34.SA","BMEB3.SA","BMEB4.SA","BMIN3.SA","BMIN4.SA","BMYB34.SA","BNBR3.SA","BNFS11.SA","BOAC34.SA","BOBR4.SA","BONY34.SA","BOVA11.SA","BOXP34.SA","BPAC11.SA","BPAC3.SA","BPAC5.SA","BPAN4.SA","BPFF11.SA","BRAP3.SA","BRAP4.SA","BRAX11.SA","BRCR11.SA","BRDT3.SA","BRFS3.SA","BRGE11.SA","BRGE12.SA","BRGE3.SA","BRGE8.SA","BRIV3.SA","BRIV4.SA","BRKM3.SA","BRKM5.SA","BRML3.SA","BRPR3.SA","BRSR3.SA","BRSR5.SA","BRSR6.SA","BSEV3.SA","BSLI3.SA","BSLI4.SA","BTLG11.SA","BTOW3.SA","BTTL3.SA","CAML3.SA","CARD3.SA","CARE11.SA","CATP34.SA","CBEE3.SA","CBOP11.SA","CCPR3.SA","CCRO3.SA","CEBR3.SA","CEBR5.SA","CEBR6.SA","CEDO3.SA","CEDO4.SA","CEED3.SA","CEGR3.SA","CEOC11.SA","CEPE5.SA","CESP3.SA","CESP5.SA","CESP6.SA","CGAS3.SA","CGAS5.SA","CGRA3.SA","CGRA4.SA","CHVX34.SA","CIEL3.SA","CLSC4.SA","CMCS34.SA","CMIG3.SA","CMIG4.SA","COCA34.SA","COCE5.SA","COGN3.SA","COLG34.SA","COPH34.SA","COTY34.SA","COWC34.SA","CPFE3.SA","CPLE3.SA","CPLE6.SA","CRDE3.SA","CRFB3.SA","CRIV3.SA","CRIV4.SA","CRPG3.SA","CRPG5.SA","CRPG6.SA","CSAN3.SA","CSCO34.SA","CSMG3.SA","CSNA3.SA","CSRN3.SA","CSRN5.SA","CSRN6.SA","CTGP34.SA","CTKA3.SA","CTKA4.SA","CTNM3.SA","CTNM4.SA","CTSA3.SA","CTSA4.SA","CTXT11.SA","CVCB3.SA","CVSH34.SA","CXCE11B.SA","CXRI11.SA","CYRE3.SA","DASA3.SA","DDNB34.SA","DEAI34.SA","DHER34.SA","DIRR3.SA","DISB34.SA","DIVO11.SA","DMMO3.SA","DOHL3.SA","DOHL4.SA","DTCY3.SA","DTEX3.SA","DUKB34.SA","EALT3.SA","EALT4.SA","EBAY34.SA","ECOO11.SA","ECOR3.SA","EDFO11B.SA","EGIE3.SA","EKTR4.SA","ELET3.SA","ELET6.SA","EMAE4.SA","EMBR3.SA","ENAT3.SA","ENBR3.SA","ENEV3.SA","ENGI11.SA","ENGI3.SA","ENGI4.SA","ENMT3.SA","ENMT4.SA","EQMA3B.SA","EQPA3.SA","EQPA5.SA","EQPA7.SA","EQTL3.SA","ESTR4.SA","ETER3.SA","EUCA3.SA","EUCA4.SA","EURO11.SA","EVEN3.SA","EXXO34.SA","EZTC3.SA","FBOK34.SA","FCFL11.SA","FCXO34.SA","FDMO34.SA","FESA3.SA","FESA4.SA","FHER3.SA","FIGS11.SA","FIIB11.SA","FIIP11B.SA","FIND11.SA","FIVN11.SA","FIXA11.SA","FLMA11.SA","FLRY3.SA","FMOF11.SA","FNOR11.SA","FPAB11.SA","FRAS3.SA","FRTA3.SA","FVPQ11.SA","GEOO34.SA","GEPA3.SA","GEPA4.SA","GFSA3.SA","GGBR3.SA","GGBR4.SA","GGRC11.SA","GILD34.SA","GMCO34.SA","GNDI3.SA","GOAU3.SA","GOAU4.SA","GOGL34.SA","GOGL35.SA","GOLL4.SA","GOVE11.SA","GPAR3.SA","GPCP3.SA","GPIV33.SA","GPRO34.SA","GPSI34.SA","GRND3.SA","GSGI34.SA","GSHP3.SA","GUAR3.SA","HAGA3.SA","HAGA4.SA","HALI34.SA","HAPV3.SA","HBOR3.SA","HBTS5.SA","HETA4.SA","HFOF11.SA","HGBS11.SA","HGCR11.SA","HGLG11.SA","HGRE11.SA","HGRU11.SA","HGTX3.SA","HOME34.SA","HOOT4.SA","HPQB34.SA","HYPE3.SA","IBMB34.SA","IGBR3.SA","IGTA3.SA","IMAB11.SA","INEP3.SA","INEP4.SA","IRBR3.SA","ISUS11.SA","ITLC34.SA","ITSA3.SA","ITSA4.SA","ITUB3.SA","ITUB4.SA","IVVB11.SA","JBDU3.SA","JBDU4.SA","JBSS3.SA","JHSF3.SA","JNJB34.SA")
ibov<-c(ibov,"JOPA3.SA","JPMC34.SA","JPSA3.SA","JSRE11.SA","KEPL3.SA","KHCB34.SA","KLBN11.SA","KLBN3.SA","KLBN4.SA","KNCR11.SA","KNHY11.SA","KNIP11.SA","KNRE11.SA","KNRI11.SA","LAME3.SA","LAME4.SA","LBRN34.SA","LCAM3.SA","LEVE3.SA","LIGT3.SA","LILY34.SA","LINX3.SA","LIPR3.SA","LLIS3.SA","LOGN3.SA","LPSB3.SA","LREN3.SA","LUPA3.SA","LUXM4.SA","MACY34.SA","MATB11.SA","MCDC34.SA","MDIA3.SA","MDLZ34.SA","MDTC34.SA","MEAL3.SA","MERC4.SA","METB34.SA","MFII11.SA","MGEL4.SA","MGFF11.SA","MGLU3.SA","MILS3.SA","MMMC34.SA","MMXM11.SA","MMXM3.SA","MNDL3.SA","MNPR3.SA","MOAR3.SA","MOSC34.SA","MOVI3.SA","MRCK34.SA","MRFG3.SA","MRVE3.SA","MSBR34.SA","MSCD34.SA","MSFT34.SA","MTIG4.SA","MTSA4.SA","MULT3.SA","MWET3.SA","MWET4.SA","MXRF11.SA","MYPK3.SA","NFLX34.SA","NIKE34.SA","NORD3.SA","NPAR11.SA","NUTR3.SA","NVHO11.SA","ODPV3.SA","OFSA3.SA","OIBR3.SA","OIBR4.SA","OMGE3.SA","ONEF11.SA","ORCL34.SA","OSXB3.SA","OUJP11.SA","PABY11.SA","PARD3.SA","PATI3.SA","PATI4.SA","PCAR3.SA","PDGR3.SA","PEAB3.SA","PEAB4.SA","PEPB34.SA","PETR3.SA","PETR4.SA","PFIZ34.SA","PFRM3.SA","PGCO34.SA","PHMO34.SA","PIBB11.SA","PINE4.SA","PLAS3.SA","PLRI11.SA","PMAM3.SA","PNVL3.SA","PNVL4.SA","POMO3.SA","POMO4.SA","PORD11.SA","POSI3.SA","PPLA11.SA","PRIO3.SA","PRSV11.SA","PSSA3.SA","PSVM11.SA","PTBL3.SA","PTNT3.SA","PTNT4.SA","QCOM34.SA","QUAL3.SA","RADL3.SA","RAIL3.SA","RANI3.SA","RAPT3.SA","RAPT4.SA","RBBV11.SA","RBDS11.SA","RBGS11.SA","RBRD11.SA","RBRF11.SA","RBRP11.SA","RBRR11.SA","RBVO11.SA","RCSL3.SA","RCSL4.SA","RDNI3.SA","RDPD11.SA","REDE3.SA","RENT3.SA","RIGG34.SA","RNEW11.SA","RNEW3.SA","RNEW4.SA","RNGO11.SA","ROMI3.SA","ROST34.SA","RPAD3.SA","RPAD5.SA","RPAD6.SA","RPMG3.SA","RSID3.SA","RYTT34.SA","SANB11.SA","SANB3.SA","SANB4.SA","SAPR11.SA","SAPR3.SA","SAPR4.SA","SBSP3.SA","SBUB34.SA","SCAR3.SA","SCHW34.SA","SCPF11.SA","SDIL11.SA","SEER3.SA","SGPS3.SA","SHOW3.SA","SHUL4.SA","SLBG34.SA","SLCE3.SA","SLED3.SA","SLED4.SA","SMAL11.SA","SMLS3.SA","SMTO3.SA","SNSY3.SA","SNSY5.SA","SPTW11.SA","SPXI11.SA","SQIA3.SA","SSFO34.SA","STBP3.SA","SULA11.SA","SULA3.SA","SULA4.SA","SUZB3.SA","TAEE11.SA","TAEE3.SA","TAEE4.SA","TASA3.SA","TASA4.SA","TCNO3.SA","TCNO4.SA","TCSA3.SA","TECN3.SA","TEKA3.SA","TEKA4.SA","TELB3.SA","TELB4.SA","TEND3.SA","TESA3.SA","TEXA34.SA","TGMA3.SA","THRA11.SA","TIMS3.SA","TMOS34.SA","TOTS3.SA","TPIS3.SA","TRIS3.SA","TRPL3.SA","TRPL4.SA","TSLA34.SA","TUPY3.SA","TWTR34.SA","TXRX3.SA","TXRX4.SA","UBSG34.SA","UCAS3.SA","UGPA3.SA","UNIP3.SA","UNIP5.SA","UNIP6.SA","UPAC34.SA","UPSS34.SA","USBC34.SA","USIM3.SA","USIM5.SA","USSX34.SA","VALE3.SA","VERZ34.SA","VISA34.SA","VISC11.SA","VIVR3.SA","VIVT3.SA","VLID3.SA","VLOE34.SA","VLOL11.SA","VOTS11.SA","VRTA11.SA","VTLT11.SA","VULC3.SA","VVAR3.SA","WALM34.SA","WEGE3.SA","WFCO34.SA","WHRL3.SA","WHRL4.SA","WIZS3.SA","WLMM3.SA","WLMM4.SA","WSON33.SA","WTSP11B.SA","XBOV11.SA","XPCM11.SA","XRXB34.SA","XTED11.SA","YDUQ3.SA")
ibov<-unique(ibov)

ibov_i= c('^BVSP')

colect=c(ibov_i,ibov)
benchmark='^BVSP'

di=Sys.Date()-WA
df=Sys.Date()

dados_ibov = BatchGetSymbols(
  tickers = colect,
  first.date = di,
  last.date = df,
  thresh.bad.data = 0.9,
  bench.ticker = benchmark,)
dados_ibov = dados_ibov$df.tickers
dados_ibov2=dlply(dados_ibov,.(ticker), function(x){rownames(x)=x$row;x$row = NULL;x})

data = dados_ibov2[[1]][,c(7,6)]
colnames(data) = c("Data", dados_ibov2[[1]][1,8])

for(i in 2:length(dados_ibov2)){
  acao = dados_ibov2[[i]][,c(7,6,5)]
  colnames(acao) = c("Data", dados_ibov2[[i]][1,8],"volume")
  fi=length(acao$volume)
  ini=fi-50
  if (mean(acao$volume[ini:fi])>=neg){
    data=merge(data, acao[,1:2], by="Data")
  }
}

colnames(data)<-gsub(x=colnames(data), pattern = '\\^', replacement = '')

#Analisando Dados e Selecionando Ações Eficientes
BETA=xts(data[,-1],order.by = data$Data)
BETA=CalculateReturns(BETA)
BETA=BETA[-1,]
BETA=BETA[,colSums(is.na(BETA))==0]

acaon<-colnames(BETA)[1]
return<-(1+mean(BETA[,1]))^252-1
risk<-sd(BETA[,1])*sqrt(252)
VaR=-abs(mean(BETA[,1])+qnorm(.95)*sd(BETA[,1]))
shp=(mean(BETA[,1]))/sd(BETA[,1])
model=lm((BETA[,1])~(BETA$BVSP))
info=summary(model)
R2=info$r.squared
BET=model[["coefficients"]][["BETA$BVSP"]]
SUP=confint(model)[2,2]
INF=confint(model)[2,1]
REL = data.frame(acaon,return,risk,shp,VaR,BET,SUP,INF,R2)

for(i in 2:dim(BETA)[2]){
  acaon<-colnames(BETA)[i]
  return<-(1+mean(BETA[,i]))^252-1
  risk<-sd(BETA[,i])*sqrt(252)
  VaR=-abs(mean(BETA[,i])+qnorm(.95)*sd(BETA[,i]))
  shp=(mean(BETA[,i]))/sd(BETA[,i])
  model=lm((BETA[,i])~(BETA$BVSP))
  info=summary(model)
  R2=info$r.squared
  BET=model[["coefficients"]][["BETA$BVSP"]]
  SUP=confint(model)[2,2]
  INF=confint(model)[2,1]
  NOVAAC=data.frame(acaon,return,risk,shp,VaR,BET,SUP,INF,R2)
  REL=rbind(REL,NOVAAC)
}
colnames(REL)<-c("Ativo","Retorno","Risco","SHP","VaR","Beta","Superior (95%)","Inferior (95%)", "R²")
REL=REL[-1,]

Dados=IBB(REL,cut_rr,cut_rk,k)

## Fronteira de Eficiência
ativos=c("Data",as.vector(Dados$Ativo))
c_return=data %>% dplyr::select(ativos)
c_return<-xts(c_return[,-1],order.by = c_return$Data)
c_return=CalculateReturns(c_return)
c_return=c_return[-1,]
mean_return=colMeans(c_return)
covMat<-cov(c_return)

xo=rep(1,dim(c_return)[2])
lw=rep(wm,dim(c_return)[2])
up=rep(1,dim(c_return)[2])

mrisk=auglag(xo,sdp,gr=NULL,lower = lw,upper = up,hin=NULL,heq = ig,localsolver = "MMA")
mret=auglag(xo,rtp,gr=NULL,lower = lw,upper = up,hin=NULL,heq = ig,localsolver = "MMA")
mshp=auglag(xo,shp,gr=NULL,lower = lw,upper = up,hin=NULL,heq = ig,localsolver = "MMA")

Final=as.data.frame(t(matrix(c(mrisk$par,1/rtp(mrisk$par),sdp(mrisk$par),1/shp(mrisk$par),
                               mret$par,1/rtp(mret$par),sdp(mret$par),1/shp(mret$par),
                               mshp$par,1/rtp(mshp$par),sdp(mshp$par),1/shp(mshp$par)),ncol=3)))
colnames(Final)=c(colnames(covMat),"Retorno","Risco","Sharpe")
rownames(Final)<-c("Mínimo Risco","Máximo Retorno","Máximo Sharpe")

## Gráfico das carteiras

c_return$maxretorno<-(as.vector(as.matrix(c_return[,1:dim(covMat)[2]])%*%t(as.matrix(Final[2,1:dim(covMat)[2]]))))
c_return$minrisk<-(as.vector(as.matrix(c_return[,1:dim(covMat)[2]])%*%t(as.matrix(Final[1,1:dim(covMat)[2]]))))
c_return$maxshp<-(as.vector(as.matrix(c_return[,1:dim(covMat)[2]])%*%t(as.matrix(Final[3,1:dim(covMat)[2]]))))

c_return[1,(dim(c_return)[2]-2):dim(c_return)[2]]<-100*(1+c_return[1,(dim(c_return)[2]-2):dim(c_return)[2]])
for (i in 2:dim(c_return)[1]) {
  i=i*1
  l=i-1
  c_return[i,(dim(c_return)[2]-2):dim(c_return)[2]]<-as.vector(c_return[l,(dim(c_return)[2]-2):dim(c_return)[2]])*as.vector(1+c_return[i,(dim(c_return)[2]-2):dim(c_return)[2]])
}

graf=melt(data.frame(data[2:dim(data)[1],1],data.frame(c_return) %>% dplyr::select(c("maxretorno","minrisk","maxshp"))), id=c("data.2.dim.data..1...1."))
colnames(graf)[1]="Data"

graf %>%
  filter(!(variable %in% "maxretorno")) %>%
  ggplot() +
  aes(x = Data, y = value, colour = variable, group = variable) +
  geom_line(size = 1L) +
  scale_color_manual(labels = c("Mínimo Risco", "Máximo Sharpe"), values = c("#034001", "#0D0D0D")) +
  labs(x = "Data", y = "Índice", title = "Comportamento do Portfólio de Ativos", caption = "Elaboração: Evânio Marques", color = "Legenda") +
  theme_minimal()

graf %>%
  filter(variable %in% "maxretorno") %>%
  ggplot() +
  aes(x = Data, y = value) +
  geom_line(size = 1L, colour="#067302") +
  labs(x = "Data", y = "Índice", title = "Comportamento do Portfólio de Ativos com Máximo retorno", caption = "Elaboração: Evânio Marques") +
  theme_minimal()