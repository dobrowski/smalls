

### Libraries -------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,janitor
        ,ggthemes
        ,knitr
        ,kableExtra
        ,formattable
        ,readxl
        ,ggalt
        ,grid
        ,scales
)


### Data Sources ------

# https://caaspp.cde.ca.gov/sb2018/research_fixfileformat18

sbac2018 <- read_delim( here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>% 
        select(`County Code`,`District Code`, `School Code`, `Test Year`, `Subgroup ID`,`Students Tested` , `Test Id`, Grade, `Percentage Standard Met and Above` ) %>%
        mutate(`County Code` = as.character(`County Code`))


subgroups <- tribble(~Subgroup,~`Subgroup ID`,
                     'All Students',	1,
                     "Male",	3,
                     "Female",	4,
                     "Fluent English proficient and English only",	6,
                     "Initial fluent English proficient (IFEP)",	7,
                     "Reclassified fluent English proficient (RFEP)" ,	8,
                     "English only",	180,
                     "English learner",	160,
                     "English learners (ELs) enrolled in school in the U.S. fewer than 12 months",	120,
                     "English learners enrolled in school in the U.S. 12 months or more",	142,
                     "Ever-ELs",	170,
                     "To be determined (TBD)",	190,
                     "Economically disadvantaged",	31,
                     "Not economically disadvantaged",	111,
                     "Black or African American Economically Disadvantaged",	200,
                     "American Indian or Alaska Native Economically Disadvantaged",	201,
                     "Asian Economically Disadvantaged",	202,
                     "Filipino Economically Disadvantaged",	203,
                     "Hispanic or Latino Economically Disadvantaged",	204,
                     "Native Hawaiian or Pacific Islander Economically Disadvantaged",	205,
                     "White Economically Disadvantaged",	206,
                     "Two or more races Economically Disadvantaged",	207,
                     "Black or African American Not Economically Disadvantaged",	220,
                     "American Indian or Alaska Native Not Economically Disadvantaged",	221,
                     "Asian Not Economically Disadvantaged",	222,
                     "Filipino Not Economically Disadvantaged",	223,
                     "Hispanic or Latino Not Economically Disadvantaged",	224,
                     "Native Hawaiian or Pacific Islander Not Economically Disadvantaged",	225,
                     "White Not Economically Disadvantaged",	226,
                     "Two or more races Not Economically Disadvantaged",	227,
                     "Students with disability",	128,
                     "Students with no reported disability",	99,
                     "Black or African American",	74,
                     "American Indian or Alaska Native",	75,
                     "Filipino",	77,
                     "Hispanic or Latino",	78,
                     "White",	80,
                     "Asian",	76,
                     "Native Hawaiian or Pacific Islander",	79,
                     "Two or more races",	144,
                     "Not a high school graduate",	90,
                     "High school graduate",	91,
                     "Some college (includes AA degree)",	92,
                     "College graduate",	93,
                     "Graduate school/Post graduate",	94,
                     "Declined to state",	121,
                     "Migrant education",	28
                     
) %>%
        mutate(Subgroup = fct_inorder(Subgroup))



sba.entity <- read_delim(here("data", "sb_ca2018entities_csv.txt"), delim = ",") %>%
        select(`County Code`,`District Code`, `School Code`, `County Name`,`District Name`, `School Name`) #%>% 
#   filter(`County Code` %in% c("00", "27"))

local <- sbac2018 %>%
        left_join(sba.entity) %>% 
        left_join(subgroups)


### Process graphics ----


# schools <- c("Bradley", "San Ardo", "San Antonio", "Chualar", "Graves")
schools <- c("Lucas")

i <- "Bradley"


for (i in schools) {
        

small <- local %>% filter(str_detect(`District Name`,i),
                 Grade == "13", # All grades )
                 `School Code` != "0000000"
                 ) %>%
        mutate(`Percentage Standard Met and Above` = as.numeric(`Percentage Standard Met and Above`) ,
               test = if_else(`Test Id` == 1, "ELA", "Math")) %>%
        na.omit()  %>%
        mutate(usesubgroup = paste0(Subgroup,"\n(",`Students Tested`,")") )

wid <- (nrow(small)/2)*.45
# wid <- 10

ggplot(small, aes(  Subgroup,  test,   fill = `Percentage Standard Met and Above` )) + 
        geom_tile(colour = "white") +
        geom_text(aes(label= percent( `Percentage Standard Met and Above`/100)), size = 3) +
 #       scale_x_discrete(labels = xlabs) +
        theme_hc() +
        scale_fill_gradient( high = "light yellow", low = "blue" )+
        theme(
                legend.position = "none",
                axis.ticks.x = element_blank(),
                strip.background = element_rect(fill = "black"),
                strip.text = element_text(colour = 'white'),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(x="Subgroup",
             y="",
             title = paste0(i, " Percent Meeting or Exceeding Standard \n by Subgroup in 2017-18"), 
             subtitle="", 
             fill="")

ggsave(here("figs", paste0(i, " Math and ELA by Subgroup.png" )), width = wid )

}



schools <- c("King", "Gonzales", "Soledad", "Greenfield", "Peninsula")

for (i in schools) {
        
        
        small <- local %>% filter(str_detect(`District Name`,i),
                                  Grade == "13", # All grades )
                                  `School Code` != "0000000"
        ) %>%
                mutate(`Percentage Standard Met and Above` = as.numeric(`Percentage Standard Met and Above`) ,
                       test = if_else(`Test Id` == 1, "ELA", "Math")) %>%
                na.omit()  %>%
                mutate(usesubgroup = paste0(Subgroup,"\n(",`Students Tested`,")") )
        
        ht <- length( unique( small$Subgroup ) ) /3
        
         wid <- length( unique( small$`School Name` ) ) + 4
        
        ggplot(small, aes(  `School Name`, fct_rev( Subgroup),  fill = `Percentage Standard Met and Above` )) + 
                geom_tile(colour = "white") +
                facet_wrap(~test) +
                geom_text(aes(label= percent( `Percentage Standard Met and Above`/100)), size = 3) +
                #       scale_x_discrete(labels = xlabs) +
                theme_hc() +
                scale_fill_gradient( high = "light yellow", low = "blue" )+
                theme(
                        legend.position = "none",
                        axis.ticks.x = element_blank(),
                        strip.background = element_rect(fill = "black"),
                        strip.text = element_text(colour = 'white'),
                        axis.text.x = element_text(angle = 45, hjust = 1)
                ) +
                labs(x="",
                     y="",
                     title = paste0(i, " Percent Meeting or Exceeding Standard \n by Subgroup in 2017-18"), 
                     subtitle="", 
                     fill="")
        
        ggsave(here("figs", paste0(i, " Math and ELA by Subgroup.png" )), width = wid , height = ht)
        
}




#### End ----