
create_marker_list <- function(convert_to_uppercase = FALSE) {
  hsc_markers <- c("Cygb", "Pparg", "Pdgfra", "Rgs5", "Ppara", "Foxf1", "Alb", 
                   "Nr1h4", "Ccn2", "Sema7a", "Wt1", "Fgf10", "Gfap", "Ngfr", 
                   "Vegfa", "Acta2", "Myb", "Des", "Fap", "Slc8a1", "Reln", 
                   "Syp", "Vcl", "Timp1", "Col1a1", "Tagln", "Col1a2", 
                   "Col3a1", "Sparc", "Rbp1", "Dcn", "Myl9", "Tpm2", "Meg3", 
                   "Bgn", "Igfbp7", "Igfbp3", "Ccn1", "Olfml3", "Igfbp6", 
                   "Ccl2", "Colec11", "Hgf")
  chol_markers <- c("Sstr2", "Slc4a2", "Krt7", "Kcnn2", "Itgb4", "Ggt6", "Cftr", 
                    "Aqp4", "Aqp1", "Pigr", "Ggt1", "Jag1", "Gpbar1", "Ggt7", 
                    "Krt19", "Onecut2", "Alpl", "Hnf1b", "Alb", "Agr2", "Tff3", 
                    "Tff1", "Sox9", "Epcam", "Cldn4", "Mmp7", "Tff2", "Scgb3a1", 
                    "Fxyd2", "Defb1", "Cd24", "Lcn2", "Cxcl1", "Cxcl6", "Lgals2", 
                    "Tacstd2", "Elf3", "Spp1", "Muc5b", "Lgals4", "Krt8")
  ec_markers <- c("Flt1", "Appbp2", "Arglu1", "Atp10d", "Bnip1", "Bst2", "Btnl9", 
                  "C11orf96", "Ccdc85b", "Ccl14", "Cd4", "Cdc73", "Cdkl1", "Chd4", 
                  "Clec1b", "Clec2b", "Clec4g", "Clec4m", "Crbn", "Crhbp", "Ctsl", 
                  "Dclre1c", "Dipk2a", "Dlc1", "Dlg1", "Dlk1", "Dnase1l3", "Dock1", 
                  "Dusp5", "Efnb2", "Eng", "F8", "Fcn2", "Fcn3", "Filip1", "Fosb", 
                  "Fxyd6", "Gbp4", "Gga2", "Gng11", "Hes1", "Hyi", "Il1r1", "Il33", 
                  "Kdr", "Kif1b", "Klhl28", "Ldb2", "Mcam", "Mcm3ap", "Mef2c", 
                  "Mfn1", "Mgat5", "Mpzl3", "Mrc1", "Oit3", "Pik3c2a", "Ppwd1", 
                  "Ptprc", "Rasgef1b", "Rasgrp3", "Reln", "Rin2", "Secisbp2l", 
                  "Stab1", "Strn3", "Tcf12", "Tex264", "Tfrc", "Trappc11", "Tspan7", 
                  "Usp48", "Vwf", "Znf286b", "Stab2", "Cd34", "Pecam1")
  hematopoietic_markers <- c("Cd34", "Ptprc")
  hepatoblast_markers <- c("Afp", "Alb", "Cebpa", "Krt14", "Dlk1", "Cdh1", "Foxm1", 
                           "Ggt1", "Hnf4a", "Prox1", "Map2k4", "Smad5", "Krt18", 
                           "Krt8", "Hnf1b", "Hhex", "Met")
  hep_markers <- c("Afp", "Hnf4a", "Krt8", "Alb", "Krt18", "Fosl1", "Eppk1", "Ucp2", 
                   "Gck", "Lrp5", "Slc10a1", "Nos2", "Atp7b", "Gjb2", "Fgfr4", 
                   "Prox1", "Crp", "Slc2a2", "Tfr2", "Kif13b", "Lipc", "Vdr", 
                   "Asgr1", "Arg1", "G6pc", "Otc", "Serpina1", "Zhx2", "Hhex", 
                   "Foxa3", "Foxa2", "Foxa1", "Cyp2e1", "Cyp1a2", "Cebpa", 
                   "Cdh1", "Gpc3", "Ahr", "Cps1", "Gls2", "Pck1", "Tat", "Wt1", 
                   "Prrg4", "Sult1a1", "Apoh", "Ctnnb1", "Abcc3", "Fgb", 
                   "Aqp3", "Plscr1", "Fga", "Apob", "Ang", "Anxa13", "Sat2", 
                   "Sfrp5", "A1cf", "Apoa1", "Bnip3", "Fgl1", "Pah", "Serpina6", 
                   "Apoa2", "Azgp1", "Fgg", "Apoc3", "Defb1", "Tm4sf4", "Gc", 
                   "Ambp", "Orm1", "Ttr", "Hal", "Ass1", "Scd", "Hmgcs1", 
                   "Acss2", "Tm7sf2", "Sec16b", "Slbp", "Rnd3", "Bche", "Ghr", 
                   "Aldh6a1", "Masp2", "Akr1c1", "Hamp", "Glul", "Acly", "Asl", 
                   "Tmem97", "Cp", "Slpi", "Acat2", "Tm4sf5", "Msmo1", "Lepr", 
                   "Rcan1", "Ar", "Rpp25l", "Hsd11b1", "Apom", "Tkfc", "G0s2", 
                   "Pon3", "C1orf53", "Ttc36", "Fst", "Mcc", "Aqp9", "Gsta2", 
                   "Nnt", "Saa4", "Mrps18c", "Ociad1", "Apoa5", "Entpd5", "C4b", 
                   "Eid2", "Tp53inp2", "Atic", "Serpinh1", "Samd5", "Grb14", 
                   "Cd3g", "Rhob", "Epb41l4b", "Gpat4", "Sptbn1", "Sdc2", 
                   "Phlda1", "Wtap", "Acadm")
  immune_markers <- c("Cd19", "Ms4a1", "Il2ra", "Ptprc", "Cd4", "Cd8a", "Cd3d", 
                      "Cd3e", "Cd3g", "Cd11c", "Cd14", "Cd16", "Cd56", "Cd16", 
                      "Cd42b", "Cd7", "Cd8", "Igd", "Cd38", "Cd24", "Cd20", 
                      "Cd14", "Cd16", "Hla-Dr", "Cd11b", "Cd11c", "Cd123", 
                      "Cd15", "Cd33", "Cd66b", "Cd38", "Cd27", "Cd94", "Cd45")
  kupffer_markers <- c("Cd14", "Cd68", "Tnf", "Lyz", "Mpo", "Vsig4", "Prok2", 
                       "Stard5", "Msr1", "Ppara", "Trem1", "Slc11a1", "Mmp13", 
                       "Vdr", "Chit1", "Osm", "Ppard", "Clec4f", "Adgre1", 
                       "Il1b", "Slc40a1", "G6pd", "Timd4", "Mnda", "Slc15a3", 
                       "Dnase1l3", "Marco", "Hfe", "Cd38", "Cd163", "Ccr5", 
                       "Clec1b", "Clec4g", "Gpihbp1", "Folr2", "Pltp", "Ftl", 
                       "Irf7", "Spic", "Csf1r", "C1qc", "C1qa", "C1qb", "Clec4e")
  liver_progenitor_markers <- c("Krt7", "Krt19")
  mesenchymal_stem_liver_markers <- c("Cd44", "Eng", "Nt5e", "Itgb1", "Thy1")
  monocytes_liver_markers <- c("Fcgr3a", "Fcgr3b", "Cd14", "Elane1")
  pericytes_wat_markers <- c("Col25a1", "Rgs6", "Kcnab1", "Thsd7b", "Myo1b", "Dlc1", 
                     "Ac003991.1", "Steap4", "Postn", "Pde1c", "Rgs5", "Adgrb3", 
                     "Ebf2", "Cacna1c", "Abcc9", "Lin7a", "Myo1b", "Stac", 
                     "Notch3", "Sgip1")
  
  #from henderson 2019 HSC dataset
  fibroblast_markers <- c(
    "Gsn", "Clec3b", "Serpinf1", "Dpt", "Fbln1", "Htra3", "Rnase4", "Cd34", "S100a6", "Igfbp6", 
    "Mgp", "Meg3", "Gas1", "Gpx3", "Aebp1", "Fth1", "Mgst1", "Nbl1", "Gas6", "Mfap4", 
    "Col1a1", "Pid1", "Pmp22", "Col1a2", "Timp2", "Nupr1", "C3", "Igf1", "Loxl1", "Igfbp4", 
    "Abca8a", "Plpp3", "Mfap5", "Col6a2", "S100a16", "Entpd2", "Fmo2", "Tppp3", "Ogn", 
    "Htra1", "Ctsh", "Pi16", "Vim", "S100a10", "Pcolce2", "Lgals1", "Emp1", "Sfrp1", "Inmt", 
    "Dpep1", "Jund", "Plxdc2", "Celf2", "Penk", "Col6a1", "Col3a1", "Sepp1", "Ftl1", "Fxyd6", 
    "Itm2a", "Col15a1", "Il11ra1", "Fbln2", "Gstm1", "Cyb5a", "Ahnak", "Tsc22d3", "Rcan2", 
    "Lsp1", "Medag", "Mmp23", "Mmp2", "Tuba1a", "Ptgis", "Smoc2", "Selm", "Mtch1", "Bicc1", 
    "Cd9", "Tmed3", "Osr1", "Ddah2", "Eln", "Lysmd2", "Lpl", "Nfix", "Ackr3", "Crispld2", 
    "Ebf1", "Lum", "Tnxb", "Itih5", "Podn", "Mfap2", "Islr", "Cilp", "S100a13", "Ltbp4", 
    "Atf5", "Fstl1", "Emp3", "Cd63", "Colec12", "Ecm2", "Scara5", "Ahnak2", "Fndc1", "Srpx", 
    "Lpar1", "Pdgfrl", "Mn1", "Pcolce", "Camk2n1", "Olfml2b", "Krtcap2", "Gabarap", "Vit", 
    "Plat", "Clmp", "Ccdc80", "Fcgrt", "Sulf2", "Tpt1", "Serpinh1", "Tmem119", "Itgbl1", 
    "Pdpn"
  )
  
  vsmc_markers <- c(
    "Tpm2", "Acta2", "Tpm1", "Myl9", "Tagln", "Myl6", "Myh11", "Crip1", "Dstn", "Pcp4l1", 
    "Mustn1", "Ckb", "Lmod1", "Bcam", "Cald1", "Ptp4a3", "Sparcl1", "Des", "Sncg", "Mylk", 
    "mt-Cytb", "mt-Co3", "mt-Atp6", "Rcan2", "Cox8a", "Tsc22d1", "Ppp1cb", "Csrp1", "mt-Nd4", 
    "Calm2", "Ptrf", "Tm4sf1", "Tinagl1", "Gng11", "Mef2c", "Prkcdbp", "Pln", "mt-Co1", 
    "Nudt4", "mt-Nd1", "Gm13889", "Malat1", "Nrip2", "Id1", "Hspb1", "Ppp1r12a", "Cox4i2", 
    "Flna", "Pdlim3", "Gapdh", "Map1lc3a", "Fxyd1", "Cnn1", "mt-Co2", "Cox6c", "Palld", 
    "Map1b", "Atpif1", "Atp5b", "Cav1", "Actg2", "Rbpms2", "Id2", "Mob2", "Actn1", "Rbpms", 
    "Ppp1r14a", "Cox6a1", "mt-Nd2", "Atp1b2", "Aspn", "Olfr558", "Tesc", "Igfbp5", "Cd200", 
    "Notch3", "Uba2", "Mgst3", "Zak", "Gpx1", "Sorbs2", "Cd9", "Cystm1", "Hcfc1r1", "Rrad", 
    "Ebf1", "Gm13861", "Rap1a", "Cox4i1", "Kcna5", "Ywhaq", "Nrarp", "Hspb6", "Ndufb9", 
    "Pbxip1", "Blmh", "Hspb2", "Filip1l", "Uqcr11", "Sh3bgr", "Rock1", "Gadd45g", "Uqcrh", 
    "Map3k7cl", "Ppia", "Asb2", "Sh3bgrl", "Atp5k", "Rasl12", "Cox7a2", "Csrp2", "Mast4", 
    "Smarcd3", "FBn1", "Btg1", "Actb", "Ndufa4l2", "Serpine2", "Atp5h", "mt-Nd3", "Fbxl22", 
    "Ptma", "Tnnt2", "Mcam", "Ppp1r12b", "Npy1r", "Uqcr10", "Thra", "Ndufb10", "Ndufa5", 
    "Slc25a5", "Msrb1", "Mapre2", "Zfhx3", "Slc25a4", "Rasl11a", "Errfi1", "Atp5d", "Ybx1", 
    "Gja4", "Cox7c", "Dynlrb1", "Tgfb1i1", "Atp5l", "H3f3b", "Lgals1", "Rasd1", "Tcap", 
    "Atp5j2", "Cox6b1", "Net1", "Tuba1c", "Dmpk", "Itpk1", "Heyl", "Ntf3", "Gucy1a3", 
    "Ptms", "Serpini1", "Aoc3", "Tpi1", "Hspb7", "Cox5b", "Col18a1", "Gpr20", "Ccdc107", 
    "Cnn2", "Lmna", "Ppp1r15a", "Chchd2", "Efhd2", "Prkar1a", "Pdgfa", "Lbh", "Filip1", "Pls3"
  )
  
  portal_hsc_markers <- c(
    "Ngfr", "Igfbp3", "Sparc", "Gcgr", "Ndufa4l2", "Tnfrsf11b", "Tagln", "Rgs4", "Ttyh1", 
    "Clca3a1", "Clec4g", "Rasl11a", "Il34", "Tmem178", "Col14a1", "Heyl", "Prss23", "Myl9", 
    "Tpm2", "Lama1", "Susd2", "Fam84a", "Gns", "Alpl", "Bcr", "Pdgfrb", "Klhl23", "Mest", 
    "Ckb", "Gpc6", "Cyp4b1", "Itgb3", "Tbx2", "Disp2", "Serpina3g", "Ccl2", "Fhl2", "Crip1", 
    "Rem1", "Hilpda", "Hsd11b1", "Gja4", "Myh11", "Trib1", "Matn2", "Tpm1", "Cxcl14", 
    "Phlda1", "Procr", "Cd248", "Htra1"
  )
  
  
  central_hsc_markers <- c(
    "Lrrn3", "Gja1", "Sprn", "Inmt", "Adm", "Robo2", "Vcam1", "Loxl1", "Tsc22d1", "Htra3", 
    "Sox4", "Sectm1a", "Adamts5", "Slc43a3", "Apoc1", "Qpct", "Lpar1", "Podn", "Gem", 
    "Ackr4", "Bmyc", "Fcna", "Lum", "Ano1", "Adamtsl2", "Rspo3", "Spon2"
  )
  
  
  
  marker_names <- ls(pattern = "_markers$")
  
  # Fetch the actual gene lists using mget
  marker_list <- mget(marker_names, envir = environment())
  
  if (convert_to_uppercase) {
    marker_list <- lapply(marker_list, toupper)
  }
  
  return(marker_list)
}

# How to use

## Source from github ##
# marker_list <- create_marker_list()

# # Display the named list of gene vectors
# print(marker_list)

# OR
# human_list <- create_marker_list(TRUE) --> will convert gene names to uppercase

