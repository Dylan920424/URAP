library(ggplot2)

entries <- df_comb %>%
  group_by(product) %>%
  count

ggplot(entries, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

services <- df_comb %>%
  group_by(service) %>%
  count

ggplot(services, aes(x=reorder(service,(n)), y=n, fill = service)) +
  xlab("service") +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

ratings <- df_comb %>%
  group_by(rating) %>%
  count

ggplot(ratings, aes(x=rating, y=n, fill = rating)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

pos <- df_comb_pos %>%
  group_by(pros_type) %>%
  count

pos <- pos[pos$pros_type != "no positive",]
pos_single <- pos[!str_detect(pos$pros_type, "&"),]
pos_double <- pos[str_detect(pos$pros_type, "&"),]

ggplot(pos, aes(x=reorder(pros_type,(n)), y=n, fill = pros_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

ggplot(pos_single, aes(x=reorder(pros_type,(n)), y=n, fill = pros_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

pie(pos_single$n, labels = pos_single$pros_type)

ggplot(pos_double, aes(x=reorder(pros_type,(n)), y=n, fill = pros_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

con <- df_comb_neg %>%
  group_by(cons_type) %>%
  count

con <- con[con$cons_type != "no negative",]
con_single <- con[!str_detect(con$cons_type, "&"),]
con_double <- con[str_detect(con$cons_type, "&"),]

ggplot(con, aes(x=reorder(cons_type,(n)), y=n, fill = cons_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

ggplot(con_single, aes(x=reorder(cons_type,(n)), y=n, fill = cons_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

pie(con$n, labels = con$cons_type)

ggplot(con_double, aes(x=reorder(cons_type,(n)), y=n, fill = cons_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()



amazon_pos <-  %>%
  group_by(pros_type) %>%
  count

amazon_pos <- amazon_pos[amazon_pos$pros_type != "no positive",]

ggplot(amazon_pos, aes(x=reorder(pros_type,(n)), y=n, fill = pros_type)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()


visual_pos <- df_comb_pos[df_comb_pos$pros_type == "visual_pos",] %>%
  group_by(product) %>%
  count

ggplot(visual_pos, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") + 
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()


thermal_pos <- df_comb_pos[df_comb_pos$pros_type == "thermal_pos",] %>%
  group_by(product) %>%
  count

ggplot(thermal_pos, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") + 
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()


control_pos <- df_comb_pos[df_comb_pos$pros_type == "control_pos",] %>%
  group_by(product) %>%
  count

ggplot(control_pos, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") + 
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()


ease_pos <- df_comb_pos[df_comb_pos$pros_type == "ease_pos",] %>%
  group_by(product) %>%
  count

ggplot(ease_pos, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") + 
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

energy_pos <- df_comb_pos[df_comb_pos$pros_type == "energy_pos",] %>%
  group_by(product) %>%
  count

energy_pos <- energy_pos[energy_pos$product != "NestThermostat",]

ggplot(energy_pos, aes(x=reorder(product,(n)), y=n, fill = product)) +
  xlab("product") + 
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

comb <- visual_pos %>% merge(thermal_pos,by="product", all=TRUE, suffixes = c("visual", "thermal")) %>% 
  merge(ease_pos,by="product", all=TRUE) %>% 
  merge(energy_pos,by="product", all=TRUE, suffixes = c("ease","energy")) %>% 
  merge(control_pos,by="product", all=TRUE) %>%
  rename(control=n, energy = nenergy, thermal = nthermal, visual = nvisual, ease = nease)

comb <- comb[comb$product!="NestThermostat",]

comb_long <- gather(comb, key="type", value="amount", c("visual", "thermal","ease","energy","control"))

ggplot(comb_long, aes(x=product, y=amount))+
  geom_bar(stat='identity', fill="forest green")+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~type, ncol=1)

# remove ease

comb <- visual_pos %>% merge(thermal_pos,by="product", all=TRUE, suffixes = c("visual", "thermal")) %>% 
  merge(control_pos,by="product", all=TRUE) %>% 
  merge(energy_pos,by="product", all=TRUE, suffixes = c("control","energy")) %>%
  rename(control = ncontrol, energy = nenergy, thermal = nthermal, visual = nvisual)

comb_long <- gather(comb, key="type", value="amount", c("visual", "thermal","control","energy"))

ggplot(comb_long, aes(x=product, y=amount))+
  geom_bar(stat='identity', fill="forest green")+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~type, ncol=1)

# remove nest

comb <- visual_pos %>% merge(thermal_pos,by="product", all=TRUE, suffixes = c("visual", "thermal")) %>% 
  merge(control_pos,by="product", all=TRUE) %>% 
  merge(energy_pos,by="product", all=TRUE, suffixes = c("control","energy")) %>%
  rename(control = ncontrol, energy = nenergy, thermal = nthermal, visual = nvisual)

comb <- comb[comb$product!="NestThermostat",]

comb_long <- gather(comb, key="type", value="amount", c("visual", "thermal","control","energy"))

ggplot(comb_long, aes(x=product, y=amount))+
  geom_bar(stat='identity', fill="forest green")+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~type, ncol=1)

#negative

visual_neg <- df_comb_neg[df_comb_neg$cons_type == "visual_neg",] %>%
  group_by(product) %>%
  count

thermal_neg <- df_comb_neg[df_comb_neg$cons_type == "thermal_neg",] %>%
  group_by(product) %>%
  count

energy_neg <- df_comb_neg[df_comb_neg$cons_type == "energy_neg",] %>%
  group_by(product) %>%
  count

ease_neg <- df_comb_neg[df_comb_neg$cons_type == "ease_neg",] %>%
  group_by(product) %>%
  count

control_neg <- df_comb_neg[df_comb_neg$cons_type == "control_neg",] %>%
  group_by(product) %>%
  count

comb <- visual_neg %>% merge(thermal_neg,by="product", all=TRUE, suffixes = c("visual", "thermal")) %>% 
  merge(ease_neg,by="product", all=TRUE) %>% 
  merge(energy_neg,by="product", all=TRUE, suffixes = c("ease","energy")) %>% 
  merge(control_neg,by="product", all=TRUE) %>%
  rename(control=n, energy = nenergy, thermal = nthermal, visual = nvisual, ease = nease)

comb <- comb[comb$product!="NestThermostat",]

comb_long <- gather(comb, key="type", value="amount", c("visual", "thermal","ease","energy","control"))

ggplot(comb_long, aes(x=product, y=amount))+
  geom_bar(stat='identity', fill="forest green")+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~type, ncol=1)

# opp

ggplot(comb_long, aes(x=type, y=amount))+
  geom_bar(stat='identity', fill="forest green")+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~product, ncol=4)
