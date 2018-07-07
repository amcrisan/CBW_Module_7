# --- FULL GGTREE ALIGNMENT

tr <- rtree(30)

d1 <- data.frame(id=tr$tip.label, val=rnorm(30, sd=3))
p <- ggtree(tr) + geom_tiplab(align = TRUE) + xlim_tree(5)

pData<-p$data %>% filter(label != "NA") %>% mutate(id = label)
d1<-inner_join(d1,pData)

#sort the ID factor by decreasing y value to be sure they're plotted in the same order

d1$id<-factor(d1$id,levels=as.character(d1$id)[order(d1$y),decreasing=TRUE])

p2<-ggplot(d1,aes(x = val, y = y)) + 
  scale_y_continuous(breaks = sort(d1$y),
                     labels = levels(d1$id))+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0,0,0),"points"))



cowplot::plot_grid(p, p2, 
                   align = "h",
                   nrow=1,
                   rel_widths = c(1, 1))

#getMaxY
tmp<-metadata%>%group_by(Year_isolated)%>%count()
ggplot(metadata, aes(x = Year_isolated)) + 
  geom_histogram(aes(fill = Phylogroup),colour="black")+
 # geom_dotplot(aes(fill=Phylogroup),colour="black",binwidth=1,size=1)+
  #ylim(c(0,max(tmp$n)))+
  theme_bw()


tree<-treeio::read.tree(file="data/kleb-pneumo-meta-tree.nwk") #reads a newick tree
tree<-ggtree::ggtree(tree)
tree%<+%metadata + geom_tippoint(aes(color=Phylogroup),alpha=0.25)

