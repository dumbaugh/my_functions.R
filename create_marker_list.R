
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
  marker_names <- ls(pattern = "_markers$")
  
  # Fetch the actual gene lists using mget
  marker_list <- mget(marker_names, envir = environment())
  
  if (convert_to_uppercase) {
    marker_list <- lapply(marker_list, toupper)
  }
  
  return(marker_list)
}

# Now, once you source this script from GitHub, you can call the function
marker_list <- create_marker_list()

# Display the named list of gene vectors
print(marker_list)

