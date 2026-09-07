suppressPackageStartupMessages({library(DynForest);library(dplyr);library(ggplot2)})
source("work/chapter5_0831/00_config.R")
fit <- readRDS(file.path(out_dir,"models","rsflc_survival.rds"))
v_path <- file.path(out_dir,"models","rsflc_native_vimp.rds")
if(file.exists(v_path)) v <- readRDS(v_path) else {
  v <- DynForest::compute_vimp(fit, IBS.min=5, IBS.max=28, ncores=8L, seed=2026L)
  saveRDS(v,v_path)
}
vals <- as.numeric(unlist(v$Importance,use.names=FALSE))
nms <- as.character(unlist(v$Inputs,use.names=FALSE))
tab <- data.frame(variable=nms,OOB_VIMP=vals) %>% arrange(desc(OOB_VIMP))
write_utf8(tab,file.path(out_dir,"tables","table_5_7_rsflc_OOB_VIMP.csv"))
ggsave(file.path(out_dir,"figures","RSFLC_01_OOB_VIMP.png"),
       ggplot(head(tab,20),aes(reorder(variable,OOB_VIMP),OOB_VIMP))+geom_col(fill="#2F75B5")+coord_flip()+theme_minimal()+labs(x=NULL,y="OOB VIMP (increase in IBS)",title="Landmark survival RSFLC variable importance"),width=7,height=5.6,dpi=300)
cat("RSFLC_VIMP_OK\n")
