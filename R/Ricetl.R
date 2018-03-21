#' @title Data Extraction for People's Republic of China Resident Identity Card
#' @description People's Republic of China resident identity card contains a lot of personal information.
#'It can extract the location of the household's residence in the identity card, the date of birth, the sex,
#'the location of the registered police station, and can identify the authenticity of the ID card through the check code.
#'Ricetl provides graphical user interface to process the function of extracting the above effective private information in the ID card number,
#'and provides a public data set related to the ID number.
#'
#' @name Ricetl
#' @aliases Ricetl
#' @author Xu Jing
#' @usage Ricetl()
#' @import tidyverse
#' @import stringr
#' @import readr
#' @import readxl
#' @import writexl
#' @import gWidgets
#' @import utils
#' @import devtools
#'
#' @examples
#'
#' library(Ricetl)
#' library(gWidgetsRGtk2)
#' Ricetl()
#'
#'
#' @export Ricetl
#'
Ricetl <- function(){
  all_data =data_source_id = NULL
  options(warn=-1)
  options(guiToolkit="RGtk2")
  ##main window
  win <- gwindow('R for Data Extraction of Resident Identity Card (PRC)--[Ricetl]',visible=FALSE)

  ##toolbar

  my_Open <-  gaction(label="Open", icon="open",handler=function(h,...){
    my_path = choose.files(caption = "Choose One File(.csv or xlsx) to -Ricetl-package")
    if(length(my_path)==0)
      galert('Please Select the Files to be Processed(A data file with a suffix of .csv or .xlsx)',title = "File Selection Problems",delay = 6)
    else{
      galert('The file you choose should be a data file with a suffix of .csv or .xlsx',title = "Tips",delay = 6)
      if(grepl("\\.csv$", my_path))
        data_source_id <- readr::read_csv(my_path)
      else{
        if(grepl("\\.xlsx$", my_path))
          data_source_id <- readxl::read_excel(my_path)
        else
          galert('Please Choose the Correct File Format (.csv or .xlsx)!',title = "File Selection Problems",delay = 6)
      }

    }
    data_source_id <<- data_source_id

  })


  my_save <- gaction(label='Save',icon='save',handler=function(h,...){
    my_path_save = choose.dir(caption = "Choose the Save Dir -Ricetl-package")

    if(!is.data.frame(all_data))
      galert('There is no output for the time, please execute the data you want to handle!',title = "File Save Failure",delay = 6)
    else{
      if(is.na(my_path_save))
        galert('Cancel the preservation!',title = "File Save Failure",delay = 6)
      else{
        readr::write_csv(all_data,path=paste0(my_path_save,'\\data_result.csv'))
        galert(paste0('The results are preserved in: ',my_path_save,'\\data_result.csv'),title = "File Save Success",delay = 6)
      }
    }

  })

  my_close <- gaction(label='Close',icon='close',handler=function(h,...){
    dispose(win)
  })

  my_about_content <- "Ricetl package is a function set with Gui to extract data from the identity card number of People's Republic of China residents, looking forward to the experience."
  my_about <- gaction(label='About',icon='about',handler=function(h,...){
    gmessage(my_about_content,title = "About Ricetl",parent=win)
  })

  my_list <- list(
    open = my_Open,
    sep = list(separator = TRUE),
    save = my_save,
    sep = list(separator = TRUE),
    close = my_close,
    sep = list(separator = TRUE),
    about = my_about,
    sep = list(separator = TRUE))


  gtb <- gWidgets::gtoolbar(toolbarlist = my_list,container = win)

  ##gframe
  gf <- gframe(horizontal=FALSE, container=win)

  ## select contant
  bg_gl <- ggroup(container = gf)

  ### select year
  gl_year <- glabel("Year:", container=bg_gl)
  year_value <- gedit(text = "2018", width = 4,  container = bg_gl)
  gseparator(horizontal = FALSE,container = bg_gl)

  ### select mising type
  gl_miss <- glabel("Missing:", container=bg_gl)
  miss_value <- gWidgets::gdroplist(items=c('NA','Mean'), selected = 1, editable = TRUE,container = bg_gl)
  gseparator(horizontal = FALSE,container = bg_gl)

  ### run my data
  gbutton("execute", container=bg_gl, handler = function(h,...) {

    #age
    if(svalue(miss_value)=='NA')
      my_age <- as.numeric(svalue(year_value)) - as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),7L,10L))
    else{
      if(svalue(miss_value)=='Mean'){
        my_age_f <- as.numeric(svalue(year_value)) - as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),7L,10L))
        my_age <- ifelse(!is.na(my_age_f),my_age_f,mean(my_age_f,na.rm = TRUE))
      }

      else{
        my_age_f <- as.numeric(svalue(year_value)) - as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),7L,10L))
        my_age <- ifelse(!is.na(my_age_f),my_age_f,svalue(miss_value))
      }
    }

    #gender

    if(svalue(miss_value)=='NA')
      my_sex <- as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),17L,17L)) %% 2
    else{
      my_sex <- as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),17L,17L)) %% 2
      galert('This option is only meaningful for filling the missing values of the age!',title = "WARN-[Ricetl]",delay = 6)
    }


    #address

    my_addr <- as.numeric(stringr::str_sub(as.vector(data.frame(data_source_id)[,1]),1L,6L))
    my_addr_2 <- sapply(my_addr,address_code)


    #Verify Result
    vrs <- lapply(as.vector(data_source_id),str_ext)
    my_vrs <- unlist(plyr::llply(vrs[[1]],VeRe))


    all_data <<- data.frame('ID_no'=data_source_id,'Address'=my_addr_2,
                            'Age'=my_age,'Gender'=my_sex,'Checkout'=my_vrs)


    my_df <- gdf(all_data, container=gf, do.subset=TRUE)


  })

  gseparator(horizontal = FALSE,container = bg_gl)


  gseparator(horizontal = TRUE,container = gf)
  gseparator(horizontal = TRUE,container = gf)



  bg_note <- ggroup(container = gf)
  gseparator(horizontal = TRUE,container = gf)
  gseparator(horizontal = TRUE,container = gf)
  gl_note <- glabel("The output of the ID card number is listed as the identity card number corresponding
to the household registration address,the corresponding age of the residents,
the corresponding sex of the residents (1: male, 0: female) and the check code,
so we can choose the year to calculate the age of residents and the missing data,
which is worth filling. And the file you open should be a data file with a suffix
of .csv or .xlsx,and the data file contains only one column, which is the identity
card number of the People's Republic of China resident",container=bg_note)



  size(win) <- c(600, 400)
  visible(win) <- TRUE


}


## Basic func for veRe
str_ext <- function(x){
  return(stringr::str_extract_all(x,pattern = ''))
}


VeRe <- function(x){
  if(length(x)!=18){
    y ='No'
  }
  else{
    y1 <- sum(as.numeric(x)[1:17]*c(7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2)) %% 11
    if(y1==0 & x[18]=='1')
      y='Yes'
    else if(y1==1 & x[18]=='0'){
      y='Yes'
    }
    else if(y1==2 & x[18]=='X'){
      y='Yes'
    }
    else if(y1==3 & x[18]=='9'){
      y='Yes'
    }
    else if(y1==4 & x[18]=='8'){
      y='Yes'
    }
    else if(y1==5 & x[18]=='7'){
      y='Yes'
    }
    else if(y1==6 & x[18]=='6'){
      y='Yes'
    }
    else if(y1==7 & x[18]=='5'){
      y='Yes'
    }
    else if(y1==8 & x[18]=='4'){
      y='Yes'
    }
    else if(y1==9 & x[18]=='3'){
      y='Yes'
    }
    else if(y1==10 & x[18]=='2'){
      y='Yes'
    }
    else
      y='No'
  }
  return(y)
}


address_code <- function(x){
  data(id_card6,envir = baseenv())
  id_card6 = id_card6
  if(sum(id_card6$b6 %in% x) == 1){
    my_addr_1 <- id_card6[which(id_card6$b6 %in% x),]$adds
  }
  else{
    my_addr_1 <- NA
  }

  my_addr_1
}



id2age_0 <- function(x,year=2018){
  #age
  if(nchar(x) ==18)
    age <- year-as.numeric(substr(x,7,10))
  else
    age <- NA
  age
}

id2gender_0 <- function(x){
  if(nchar(x) == 18){
    if (as.numeric(substr(x,17,17))%%2 == 0)
      gender <- 'F'
    else
      gender <- 'M'
  }
  else
    gender <- NA

  gender
}


id2address_0 <- function(x){
  #address
  if(nchar(x) == 18){
    my_addr <- as.numeric(stringr::str_sub(as.vector(x),1L,6L))
    my_addr_2 <- sapply(my_addr,address_code)
  }
  else
    my_addr_2 <- NA
}


id2verify_0 <- function(x){
  #Verify Result
  vrs <- unlist(stringr::str_extract_all(x,pattern = ''))
  my_vrs <-VeRe(vrs)
  my_vrs
}


#' @title Extraction Age
#' @name id2age
#' @author Xu Jing
#' @description Extraction of age in the resident identity cards of People's Republic of China.
#' @aliases id2age
#' @usage id2age(x,year=2018,fillna=NA)
#' @param x A vector containing 18 Bit ID numbers.
#' @param year Calculate the reference time of age,the default value is 2018.
#' @param fillna The filling value of the missing value,the default value is \emph{NA}
#' @examples
#' library(Ricetl)
#' x <- c('654003198111200241','341881197709275718',
#' '340503199401248097','469030199406204103','511100198')
#' id2age(x)
#' id2age(x,year=2018,fillna=25)
#'
#'
#' @export id2age
#'


id2age <- function(x,year=2018,fillna=NA){
  temp <- as.vector(sapply(x,id2age_0,year=year))
  tempf <- ifelse(is.na(temp),fillna,temp)
  tempf
}


#' @title Extraction Gender
#' @name id2gender
#' @author Xu Jing
#' @description Extraction of gender in the resident identity cards of People's Republic of China.
#' @aliases id2gender
#' @usage id2gender(x,fillna=NA)
#' @param x A vector containing 18 Bit ID numbers,\link{id2age}.
#' @param fillna The filling value of the missing value,the default value is \emph{NA},\link{id2age}.
#' @return Return 'F' means that the corresponding identity card for female citizens,'M' means male.
#' @examples
#'
#' library(Ricetl)
#' x <- c('654003198111200241','341881197709275718',
#' '340503199401248097','469030199406204103','5111001')
#' id2gender(x)
#' id2gender(x,fillna=25)
#'
#' @export id2gender
#'



id2gender <- function(x,fillna=NA){
  temp <- as.vector(sapply(x,id2gender_0))
  tempf <- ifelse(is.na(temp),fillna,temp)
  tempf
}



#' @title Extraction Address
#' @name id2address
#' @author Xu Jing
#' @description Extraction of age in the resident identity cards of People's Republic of China.
#' @aliases id2address
#' @usage id2address(x,fillna=NA)
#' @param x A vector containing 18 Bit ID numbers,\link{id2age}.
#' @param fillna The filling value of the missing value,the default value is \emph{NA},\link{id2age}.
#'
#' @examples
#'
#' library(Ricetl)
#' x <- c('654003198111200241','341881197709275718',
#' '340503199401248097','469030199406204103','51110019')
#' id2address(x)
#' id2address(x,fillna='Error')
#'
#' @export id2address
#'


id2address <- function(x,fillna=NA){
  temp <- as.vector(sapply(x,id2address_0))
  tempf <- ifelse(is.na(temp),fillna,temp)
  tempf
}



#' @title To Determine Whether an ID Number is a Real ID Number
#' @name id2verify
#' @author Xu Jing
#' @description To determine whether an ID number is a real ID number.
#' @aliases id2verify
#' @usage id2verify(x)
#' @param x A vector containing 18 Bit ID numbers,\link{id2age}.
#' @examples
#'
#' library(Ricetl)
#' x <- c('654003198111200241','341881197709275718',
#' '340503199401248097','469030199406204103','51110019')
#' id2verify(x)
#'
#' @export id2verify
#'

id2verify <- function(x){
  temp <- as.vector(sapply(x,id2verify_0))
  temp
}


#' @title Data Set for Address Correspondence
#' @name id_card6
#' @author Xu Jing
#' @description The form of the corresponding relationship between the
#' first six places of the ID card and the address of the household registration.
#' @aliases id_card6
#' @docType data
#' @usage data(id_card6)
#' @examples
#' library(Ricetl)
#' data(id_card6)
NULL



