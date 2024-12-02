library(ggplot2)
library(dplyr)
library(magrittr)

# Rho = 1e-8----

ibd_segments_w_ped_poly_rho_1e_8 = read.csv('oeb252_2024/ibd_segments_w_ped_poly_rho_1e-8.csv')
ibd_segments_w_ped_poly_rho_1e_8$method = 'Polygamous, With Pedigree, rho = 1e-8'

pairwise_ibd_w_ped_poly_rho_1e_8 = read.csv('oeb252_2024/pairwise_ibd_w_ped_poly_rho_1e-8.csv')
pairwise_ibd_w_ped_poly_rho_1e_8$method = 'Polygamous, With Pedigree, rho = 1e-8'

ibd_segments_w_ped_rho_1e_8 = read.csv('oeb252_2024/ibd_segments_w_ped_rho_1e-8.csv')
ibd_segments_w_ped_rho_1e_8$method = 'Monogamous, With Pedigree, rho = 1e-8'

pairwise_ibd_w_ped_rho_1e_8 = read.csv('oeb252_2024/pairwise_ibd_w_ped_rho_1e-8.csv')
pairwise_ibd_w_ped_rho_1e_8$method = 'Monogamous, With Pedigree, rho = 1e-8'

ibd_segments_w_ped_bigf_rho_1e_8 = read.csv('oeb252_2024/ibd_segments_w_ped_bigf_rho_1e-8.csv')
ibd_segments_w_ped_bigf_rho_1e_8$method = 'With Pedigree and big families, rho = 1e-8'

ibd_segments_w_ped_bigf_rho_1e_8 = ibd_segments_w_ped_bigf_rho_1e_8[
  sample(1:nrow(ibd_segments_w_ped_bigf_rho_1e_8), 
         round(0.1*(nrow(ibd_segments_w_ped_bigf_rho_1e_8))), replace = F),]

pairwise_ibd_w_ped_bigf_rho_1e_8 = read.csv('oeb252_2024/pairwise_ibd_w_ped_bigf_rho_1e-8.csv')
pairwise_ibd_w_ped_bigf_rho_1e_8$method = 'With Pedigree and big families, rho = 1e-8'

ibd_segments_wo_ped_rho_1e_8 = read.csv('oeb252_2024/ibd_segments_wo_ped_rho_1e-8.csv')
ibd_segments_wo_ped_rho_1e_8$method = 'Without Pedigree, rho = 1e-8'


ibd_segments_wo_ped_rho_1e_8 = ibd_segments_wo_ped_rho_1e_8[
  sample(1:nrow(ibd_segments_wo_ped_rho_1e_8), 
         round(0.01*(nrow(ibd_segments_wo_ped_rho_1e_8))), replace = F),]

pairwise_ibd_wo_ped_rho_1e_8 = read.csv('oeb252_2024/pairwise_ibd_wo_ped_rho_1e-8.csv')
pairwise_ibd_wo_ped_rho_1e_8$method = 'Without Pedigree, rho = 1e-8'

## Draw IBD length distribution for rho = 1e-8----
ibd_segments_rho8 = rbind(ibd_segments_w_ped_poly_rho_1e_8,
                          ibd_segments_w_ped_rho_1e_8,
                     ibd_segments_w_ped_bigf_rho_1e_8,
                     ibd_segments_wo_ped_rho_1e_8)

ibd_segments_rho8 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-8',
                                    'Polygamous, With Pedigree, rho = 1e-8',
                                    'Monogamous, With Pedigree, rho = 1e-8',
                                    'With Pedigree and big families, rho = 1e-8'
                                    )))

ibd_segments_rho8 %>%
  ggplot(aes(x = ibd_lengths))+
  geom_histogram(binwidth = 10000)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Count')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))


ibd_segments_rho8 %>%
  ggplot(aes(x = ibd_lengths, y = ibd_nodes - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))



pairwise_ibd_rho8 = rbind(pairwise_ibd_w_ped_poly_rho_1e_8,
                          pairwise_ibd_w_ped_rho_1e_8,
                          pairwise_ibd_w_ped_bigf_rho_1e_8,
                          pairwise_ibd_wo_ped_rho_1e_8)

pairwise_ibd_rho8 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-8',
                                    'Polygamous, With Pedigree, rho = 1e-8',
                                    'Monogamous, With Pedigree, rho = 1e-8',
                                    'With Pedigree and big families, rho = 1e-8'
                         )))


pairwise_ibd_rho8 %>%
  ggplot(aes(x = ibd_frac, y = ibd_min_node - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method ~., ncol = 1, scales = 'free_y')+
  labs(x = 'Total fraction of IBD',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))

# Rho = 1e-7----

ibd_segments_w_ped_poly_rho_1e_7 = read.csv('oeb252_2024/ibd_segments_w_ped_poly_rho_1e-7.csv')
ibd_segments_w_ped_poly_rho_1e_7$method = 'Polygamous, With Pedigree, rho = 1e-7'

pairwise_ibd_w_ped_poly_rho_1e_7 = read.csv('oeb252_2024/pairwise_ibd_w_ped_poly_rho_1e-7.csv')
pairwise_ibd_w_ped_poly_rho_1e_7$method = 'Polygamous, With Pedigree, rho = 1e-7'

ibd_segments_w_ped_rho_1e_7 = read.csv('oeb252_2024/ibd_segments_w_ped_rho_1e-7.csv')
ibd_segments_w_ped_rho_1e_7$method = 'Monogamous, With Pedigree, rho = 1e-7'

pairwise_ibd_w_ped_rho_1e_7 = read.csv('oeb252_2024/pairwise_ibd_w_ped_rho_1e-7.csv')
pairwise_ibd_w_ped_rho_1e_7$method = 'Monogamous, With Pedigree, rho = 1e-7'

ibd_segments_w_ped_bigf_rho_1e_7 = read.csv('oeb252_2024/ibd_segments_w_ped_bigf_rho_1e-7.csv')
ibd_segments_w_ped_bigf_rho_1e_7$method = 'With Pedigree and big families, rho = 1e-7'

pairwise_ibd_w_ped_bigf_rho_1e_7 = read.csv('oeb252_2024/pairwise_ibd_w_ped_bigf_rho_1e-7.csv')
pairwise_ibd_w_ped_bigf_rho_1e_7$method = 'With Pedigree and big families, rho = 1e-7'

ibd_segments_wo_ped_rho_1e_7 = read.csv('oeb252_2024/ibd_segments_wo_ped_rho_1e-7.csv')
ibd_segments_wo_ped_rho_1e_7$method = 'Without Pedigree, rho = 1e-7'

pairwise_ibd_wo_ped_rho_1e_7 = read.csv('oeb252_2024/pairwise_ibd_wo_ped_rho_1e-7.csv')
pairwise_ibd_wo_ped_rho_1e_7$method = 'Without Pedigree, rho = 1e-7'

## Draw IBD length distribution for rho = 1e-7----
ibd_segments_rho7 = rbind(ibd_segments_w_ped_poly_rho_1e_7,
                          ibd_segments_w_ped_rho_1e_7,
                          ibd_segments_w_ped_bigf_rho_1e_7,
                          ibd_segments_wo_ped_rho_1e_7)

ibd_segments_rho7 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-7',
                                    'Polygamous, With Pedigree, rho = 1e-7',
                                    'Monogamous, With Pedigree, rho = 1e-7',
                                    'With Pedigree and big families, rho = 1e-7'
                         )))

ibd_segments_rho7 %>%
  ggplot(aes(x = ibd_lengths))+
  geom_histogram(binwidth = 10000)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Count')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))


ibd_segments_rho7 %>%
  ggplot(aes(x = ibd_lengths, y = ibd_nodes - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))

pairwise_ibd_rho7 = rbind(pairwise_ibd_w_ped_poly_rho_1e_7,
                          pairwise_ibd_w_ped_rho_1e_7,
                          pairwise_ibd_w_ped_bigf_rho_1e_7,
                          pairwise_ibd_wo_ped_rho_1e_7)

pairwise_ibd_rho7 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-7',
                                    'Polygamous, With Pedigree, rho = 1e-7',
                                    'Monogamous, With Pedigree, rho = 1e-7',
                                    'With Pedigree and big families, rho = 1e-7'
                         )))


pairwise_ibd_rho7 %>%
  ggplot(aes(x = ibd_frac, y = ibd_min_node - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method ~., ncol = 1, scales = 'free_y')+
  labs(x = 'Total fraction of IBD',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))

# Rho = 1e-6----

ibd_segments_w_ped_poly_rho_1e_6 = read.csv('oeb252_2024/ibd_segments_w_ped_poly_rho_1e-6.csv')
ibd_segments_w_ped_poly_rho_1e_6$method = 'Polygamous, With Pedigree, rho = 1e-6'

pairwise_ibd_w_ped_poly_rho_1e_6 = read.csv('oeb252_2024/pairwise_ibd_w_ped_poly_rho_1e-6.csv')
pairwise_ibd_w_ped_poly_rho_1e_6$method = 'Polygamous, With Pedigree, rho = 1e-6'

ibd_segments_w_ped_rho_1e_6 = read.csv('oeb252_2024/ibd_segments_w_ped_rho_1e-6.csv')
ibd_segments_w_ped_rho_1e_6$method = 'Monogamous, With Pedigree, rho = 1e-6'

pairwise_ibd_w_ped_rho_1e_6 = read.csv('oeb252_2024/pairwise_ibd_w_ped_rho_1e-6.csv')
pairwise_ibd_w_ped_rho_1e_6$method = 'Monogamous, With Pedigree, rho = 1e-6'

ibd_segments_w_ped_bigf_rho_1e_6 = read.csv('oeb252_2024/ibd_segments_w_ped_bigf_rho_1e-6.csv')
ibd_segments_w_ped_bigf_rho_1e_6$method = 'With Pedigree and big families, rho = 1e-6'

pairwise_ibd_w_ped_bigf_rho_1e_6 = read.csv('oeb252_2024/pairwise_ibd_w_ped_bigf_rho_1e-6.csv')
pairwise_ibd_w_ped_bigf_rho_1e_6$method = 'With Pedigree and big families, rho = 1e-6'

ibd_segments_wo_ped_rho_1e_6 = read.csv('oeb252_2024/ibd_segments_wo_ped_rho_1e-6.csv')
ibd_segments_wo_ped_rho_1e_6$method = 'Without Pedigree, rho = 1e-6'

pairwise_ibd_wo_ped_rho_1e_6 = read.csv('oeb252_2024/pairwise_ibd_wo_ped_rho_1e-6.csv')
pairwise_ibd_wo_ped_rho_1e_6$method = 'Without Pedigree, rho = 1e-6'

## Draw IBD length distribution for rho = 1e-6----
ibd_segments_rho6 = rbind(ibd_segments_w_ped_poly_rho_1e_6,
                          ibd_segments_w_ped_rho_1e_6,
                          ibd_segments_w_ped_bigf_rho_1e_6,
                          ibd_segments_wo_ped_rho_1e_6)

ibd_segments_rho6 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-6',
                                    'Polygamous, With Pedigree, rho = 1e-6',
                                    'Monogamous, With Pedigree, rho = 1e-6',
                                    'With Pedigree and big families, rho = 1e-6'
                         )))

ibd_segments_rho6 %>%
  ggplot(aes(x = ibd_lengths))+
  geom_histogram(binwidth = 10000)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Count')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))


ibd_segments_rho6 %>%
  ggplot(aes(x = ibd_lengths, y = ibd_nodes - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))

pairwise_ibd_rho6 = rbind(pairwise_ibd_w_ped_poly_rho_1e_6,
                          pairwise_ibd_w_ped_rho_1e_6,
                          pairwise_ibd_w_ped_bigf_rho_1e_6,
                          pairwise_ibd_wo_ped_rho_1e_6)

pairwise_ibd_rho6 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-6',
                                    'Polygamous, With Pedigree, rho = 1e-6',
                                    'Monogamous, With Pedigree, rho = 1e-6',
                                    'With Pedigree and big families, rho = 1e-6'
                         )))


pairwise_ibd_rho6 %>%
  ggplot(aes(x = ibd_frac, y = ibd_min_node - 2000))+
  geom_point(alpha = 0.01)+
  facet_wrap(method ~., ncol = 1, scales = 'free_y')+
  labs(x = 'Total fraction of IBD',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))


# Rho = 1e-5----

ibd_segments_w_ped_poly_rho_1e_5 = read.csv('oeb252_2024/ibd_segments_w_ped_poly_rho_1e-5.csv')
ibd_segments_w_ped_poly_rho_1e_5$method = 'Polygamous, With Pedigree, rho = 1e-5'

pairwise_ibd_w_ped_poly_rho_1e_5 = read.csv('oeb252_2024/pairwise_ibd_w_ped_rho_1e-5.csv')
pairwise_ibd_w_ped_poly_rho_1e_5$method = 'Polygamous, With Pedigree, rho = 1e-5'

ibd_segments_w_ped_rho_1e_5 = read.csv('oeb252_2024/ibd_segments_w_ped_rho_1e-5.csv')
ibd_segments_w_ped_rho_1e_5$method = 'Monogamous, With Pedigree, rho = 1e-5'

pairwise_ibd_w_ped_rho_1e_5 = read.csv('oeb252_2024/pairwise_ibd_w_ped_rho_1e-5.csv')
pairwise_ibd_w_ped_rho_1e_5$method = 'Monogamous, With Pedigree, rho = 1e-5'

ibd_segments_w_ped_bigf_rho_1e_5 = read.csv('oeb252_2024/ibd_segments_w_ped_bigf_rho_1e-5.csv')


ibd_segments_w_ped_bigf_rho_1e_5_backup = ibd_segments_w_ped_bigf_rho_1e_5

ibd_segments_w_ped_bigf_rho_1e_5 = ibd_segments_w_ped_bigf_rho_1e_5[
  sample(1:nrow(ibd_segments_w_ped_bigf_rho_1e_5), 
         round(0.01*(nrow(ibd_segments_w_ped_bigf_rho_1e_5))), replace = F),]

ibd_segments_w_ped_bigf_rho_1e_5$method = 'With Pedigree and big families, rho = 1e-5'

pairwise_ibd_w_ped_bigf_rho_1e_5 = read.csv('oeb252_2024/pairwise_ibd_w_ped_bigf_rho_1e-5.csv')
pairwise_ibd_w_ped_bigf_rho_1e_5$method = 'With Pedigree and big families, rho = 1e-5'

ibd_segments_wo_ped_rho_1e_5 = read.csv('oeb252_2024/ibd_segments_wo_ped_rho_1e-5.csv')
ibd_segments_wo_ped_rho_1e_5$method = 'Without Pedigree, rho = 1e-5'

pairwise_ibd_wo_ped_rho_1e_5 = read.csv('oeb252_2024/pairwise_ibd_wo_ped_rho_1e-5.csv')
pairwise_ibd_wo_ped_rho_1e_5$method = 'Without Pedigree, rho = 1e-5'

nrow(ibd_segments_rho5)

## Draw IBD length distribution for rho = 1e-5----
ibd_segments_rho5 = rbind(ibd_segments_w_ped_poly_rho_1e_5,
                          ibd_segments_w_ped_rho_1e_5,
                          ibd_segments_w_ped_bigf_rho_1e_5,
                          ibd_segments_wo_ped_rho_1e_5)

ibd_segments_rho5 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-5',
                                    'Polygamous, With Pedigree, rho = 1e-5',
                                    'Monogamous, With Pedigree, rho = 1e-5',
                                    'With Pedigree and big families, rho = 1e-5'
                         )))

ibd_segments_rho5 %>%
  ggplot(aes(x = ibd_lengths))+
  geom_histogram(binwidth = 5000)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Count')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))


ibd_segments_rho5 %>%
  ggplot(aes(x = ibd_lengths, y = ibd_nodes - 2000))+
  geom_point(alpha = 0.3)+
  facet_wrap(method~., ncol = 1, scales = 'free_y')+
  labs(x = 'Size of IBD block',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))

pairwise_ibd_rho5 = rbind(pairwise_ibd_w_ped_poly_rho_1e_5,
                          pairwise_ibd_w_ped_rho_1e_5,
                          pairwise_ibd_w_ped_bigf_rho_1e_5,
                          pairwise_ibd_wo_ped_rho_1e_5)

pairwise_ibd_rho5 %<>%
  mutate(method = factor(method,
                         levels = c('Without Pedigree, rho = 1e-5',
                                    'Polygamous, With Pedigree, rho = 1e-5',
                                    'Monogamous, With Pedigree, rho = 1e-5',
                                    'With Pedigree and big families, rho = 1e-5'
                         )))


pairwise_ibd_rho5 %>%
  ggplot(aes(x = ibd_frac, y = ibd_min_node - 2000))+
  geom_point(alpha = 0.01)+
  facet_wrap(method ~., ncol = 1, scales = 'free_y')+
  labs(x = 'Total fraction of IBD',
       y = 'Time')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))
