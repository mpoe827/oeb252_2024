
# Function build_random_pedigree----

build_random_pedigree = function(
    N = 100,
    n = round(0.3 * N),
    times = (round(2 * log2(N)) - 1),
    partner_selection = 'monogamy',
    f_ind_in_big_families = 0.4,
    exp_big_family_size = 10
){
  
  library(dplyr)
  library(magrittr)
  library(svMisc)
  
  times = 0:times
  Pop_labels = 0:(N-1)
  Sample_labels = 0:(n-1)
  
  # Expected number of individuals in big families
  n_ind_bigfamilies = N*f_ind_in_big_families
  
  # Expected number of unique parent pairs for big families
  n_parentpairs_for_big_families = round(n_ind_bigfamilies/exp_big_family_size)
  
  pedigree_table = NULL
  
  print('Starting to build pedigree')
  
  start_time = Sys.time()
  
  for(time in times){
    
    # Define labels of individuals and parents in time t
    time_t_ind_labels = Pop_labels + time * N
    time_t_parent_labels = Pop_labels + (time + 1) * N
    
    if(f_ind_in_big_families > 0 & exp_big_family_size > 0){
      
      # select individuals that belongs to bigfamilies and regular families
      time_t_bigfamily_members = sample(time_t_ind_labels, n_ind_bigfamilies, replace = F)
      time_t_normalfamily_members = time_t_ind_labels[!(time_t_ind_labels %in% time_t_bigfamily_members)]
      
      # Define the parent pairs for the big families
      big_family_parent_pairs = NULL
      big_family_parent_labels = time_t_parent_labels
      
      for(i in 1:n_parentpairs_for_big_families){
        big_family_parent_pairs = rbind(big_family_parent_pairs, sample(big_family_parent_labels, 2, replace = F))
        big_family_parent_labels = time_t_parent_labels[!(time_t_parent_labels %in% c(big_family_parent_pairs))]
      }
      
      normal_family_parent_labels = big_family_parent_labels
      
      big_family_parent_labels = NULL
      big_family_pedigree_df = NULL
      
      # Asign individuals to each parent in big families
      
      for(ind in time_t_bigfamily_members){
        
        parent_pair = big_family_parent_pairs[sample(1:nrow(big_family_parent_pairs), 1),]
        
        big_family_pedigree_df = rbind(
          big_family_pedigree_df,
          data.frame(id = ind,
                     parent0 = parent_pair[1], 
                     parent1 = parent_pair[2],
                     time = time,
                     is_sample = ifelse(time == 0, 1, 0)))
        
        
      }
      
    }else{
      
      time_t_normalfamily_members = time_t_ind_labels
      normal_family_parent_labels = time_t_parent_labels
      
    }
    
    
    
    # Define regular parent pairs for monogamous or polygamous populations
    
    if(tolower(partner_selection) == 'monogamy'){
      monogamous_normal_family_parent_pairs = NULL
      
      number_of_normail_pairs = floor(length(normal_family_parent_labels)/2)
      
      for(i in 1:number_of_normail_pairs){
        
        parent_pair = sample(normal_family_parent_labels, 2, replace = F)
        
        monogamous_normal_family_parent_pairs = rbind(
          monogamous_normal_family_parent_pairs,
          parent_pair
        )
        
        normal_family_parent_labels = normal_family_parent_labels[!(normal_family_parent_labels %in% parent_pair)]
        
      }
    }
    
    normal_family_pedigree_df = NULL
    
    for(ind in time_t_normalfamily_members){
      
      if(tolower(partner_selection) == 'monogamy'){
        
        selected_parent_pair = sample(1:nrow(monogamous_normal_family_parent_pairs), 1)
        
        parent_pair = monogamous_normal_family_parent_pairs[selected_parent_pair,]
        
        normal_family_pedigree_df = rbind(
          normal_family_pedigree_df,
          data.frame(id = ind,
                     parent0 = parent_pair[1], 
                     parent1 = parent_pair[2],
                     time = time,
                     is_sample = ifelse(time == 0, 1, 0)))
        
        
      }else if(tolower(partner_selection) == 'polygamy'){
        
        parent_pair = sample(time_t_parent_labels, 2, replace = F)
        
        normal_family_pedigree_df = rbind(
          normal_family_pedigree_df,
          data.frame(id = ind,
                     parent0 = parent_pair[1], 
                     parent1 = parent_pair[2],
                     time = time,
                     is_sample = ifelse(time == 0, 1, 0)))
        
      }
    }
    
    if(f_ind_in_big_families > 0 & exp_big_family_size > 0){
      pop_pedigree_table = rbind(normal_family_pedigree_df,
                                 big_family_pedigree_df)
    }else{
      pop_pedigree_table = normal_family_pedigree_df
    }
    
    pop_pedigree_table %<>% arrange(id)
    
    if(time == 0){
      sample_pedigree_table = pop_pedigree_table[sample(1:nrow(pop_pedigree_table), n),]
    }else{
      sample_pedigree_table = pop_pedigree_table[pop_pedigree_table$id %in% samples_for_next_generation,]
    }
    
    if(time == times[length(times)]){
      
      sample_pedigree_table = data.frame(id = samples_for_next_generation,
                                         parent0 = '.', 
                                         parent1 = '.',
                                         time = time + 1,
                                         is_sample = 0)
      
    }
    
    
    samples_for_next_generation = unique(c(sample_pedigree_table$parent0, sample_pedigree_table$parent1))
    
    
    sample_pedigree_table %<>% arrange(id)
    
    pedigree_table = rbind(
      pedigree_table,
      sample_pedigree_table)
    
    progress(round(100*(time + 1)/length(times)))
    
  }
  
  end_time = Sys.time()
  
  print('Pedigree was build in:')
  print(end_time - start_time)
  
  
  print('Starting to write pedigree as a string')
  
  start_time = Sys.time()
  pedigree_table_string = "# id parent0 parent1 time is_sample"
  
  # for(line in 1:nrow(pedigree_table)){
  #   
  #   pedigree_table_string = paste(pedigree_table_string, paste(pedigree_table[line, ], collapse = '\t'), sep = '\n')
  #   
  #   progress(round(100*(line)/nrow(pedigree_table)))
  #   
  # }
  
  end_time = Sys.time()
  
  print('Pedigree was written in:')
  print(end_time - start_time)
  
  return(list(pedigree_table_string = pedigree_table_string,
              pedigree_table = pedigree_table
  ))
  
}

# Parameters to test----

## Fixed pedigree without big families----

# N = 10000
# n = round(0.1 * N)
# times = (round(2 * log2(N)) - 1)
# partner_selection = 'monogamy'
# f_ind_in_big_families = 0
# exp_big_family_size = 0
# 
# pedigree_without_bigfamilies = build_random_pedigree(
#   N = N,
#   n = n,
#   times = times,
#   partner_selection = partner_selection,
#   f_ind_in_big_families = f_ind_in_big_families,
#   exp_big_family_size = exp_big_family_size
# )



N = 1000
n = round(1 * N)
times = (round(2 * log2(N)) - 1)
partner_selection = 'polygamy'
f_ind_in_big_families = 0
exp_big_family_size = 0

pedigree_poly_without_bigfamilies_1000_1 = build_random_pedigree(
  N = N,
  n = n,
  times = times,
  partner_selection = partner_selection,
  f_ind_in_big_families = f_ind_in_big_families,
  exp_big_family_size = exp_big_family_size
)

pedigree_poly_without_bigfamilies_1000_1$pedigree_table %>%
  filter(time == 0) %>% 
  summarise(nOffsprings = n(), .by = c(parent0)) %>% 
  nrow()

expected_n_offsprings_poly_without_bigfam = rbind(data.frame(parent0 = 'A',
                                                        parent1 = 'B',
                                                        nOffsprings = rep(0, 1000 - pedigree_poly_without_bigfamilies_1000_1$pedigree_table %>%
                                                                            filter(time == 0) %>%
                                                                            summarise(nOffsprings = n(), .by = c(parent0)) %>%
                                                                            nrow())),
                                                  pedigree_poly_without_bigfamilies_1000_1$pedigree_table %>%
                                               #filter(time != 20) %>%
                                               filter(time == 0) %>%
                                               summarise(nOffsprings = n(),
                                                         parent1 = 'B', .by = c(parent0))
)



expected_n_offsprings_poly_without_bigfam$method = 'Polygamy, without big fam.'


N = 1000
n = round(1 * N)
times = (round(2 * log2(N)) - 1)
partner_selection = 'monogamy'
f_ind_in_big_families = 0
exp_big_family_size = 0

pedigree_without_bigfamilies_1000_1 = build_random_pedigree(
  N = N,
  n = n,
  times = times,
  partner_selection = partner_selection,
  f_ind_in_big_families = f_ind_in_big_families,
  exp_big_family_size = exp_big_family_size
)

pedigree_without_bigfamilies_1000_1$pedigree_table %>%
  filter(time == 0) %>% 
  summarise(nOffsprings = n(), .by = c(parent0, parent1)) %>% 
  nrow()

expected_n_offsprings_without_bigfam = rbind(data.frame(parent0 = 'A',
           parent1 = 'B',
           nOffsprings = rep(0, 500 - pedigree_without_bigfamilies_1000_1$pedigree_table %>%
                               filter(time == 0) %>% 
                               summarise(nOffsprings = n(), .by = c(parent0, parent1)) %>% 
                               nrow())),
      pedigree_without_bigfamilies_1000_1$pedigree_table %>% 
        #filter(time != 20) %>%
        filter(time == 0) %>%
        summarise(nOffsprings = n(), .by = c(parent0, parent1))
      )

expected_n_offsprings_without_bigfam$method = 'Monogamy, without big fam.'

  




## Fixed pedigree without big families----

N = 1000
n = round(1 * N)
times = (round(2 * log2(N)) - 1)
partner_selection = 'monogamy'
f_ind_in_big_families = 0.5
exp_big_family_size = 20

pedigree_with_bigfamilies_1000_1 = build_random_pedigree(
  N = N,
  n = n,
  times = times,
  partner_selection = partner_selection,
  f_ind_in_big_families = f_ind_in_big_families,
  exp_big_family_size = exp_big_family_size
)


expected_n_offsprings_with_bigfam = rbind(data.frame(parent0 = 'A',
                                                        parent1 = 'B',
                                                        nOffsprings = rep(0, 500 - pedigree_with_bigfamilies_1000_1$pedigree_table %>%
                                                                            filter(time == 0) %>% 
                                                                            summarise(nOffsprings = n(), .by = c(parent0, parent1)) %>% 
                                                                            nrow())),
                                         pedigree_with_bigfamilies_1000_1$pedigree_table %>% 
                                               #filter(time != 20) %>%
                                               filter(time == 0) %>%
                                               summarise(nOffsprings = n(), .by = c(parent0, parent1))
)

expected_n_offsprings_with_bigfam$method = 'Monogamy, with big fam.'

expected_n_offsprings = rbind(expected_n_offsprings_poly_without_bigfam,
                              expected_n_offsprings_without_bigfam,
                              expected_n_offsprings_with_bigfam)

library(ggplot2)

expected_n_offsprings %>%
  mutate(method = factor(method, levels = 
                           c('Polygamy, without big fam.',
                             'Monogamy, without big fam.',
                             'Monogamy, with big fam.')))%>%
  ggplot(aes(x = nOffsprings)) + 
  geom_histogram(binwidth = 1)+
  facet_grid(.~method) + 
  labs(x = 'Number of offsprings',
       y = 'Count')+
  theme_bw()+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10))




