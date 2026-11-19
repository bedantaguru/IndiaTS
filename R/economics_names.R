

econ_common_short_forms <- function(mapping = NULL){

  gdp_name_mapping <- c(
    # Core Macro Aggregates
    "Gross Domestic Product" = "GDP",
    "Gross Value Added" = "GVA",
    "AGVA" = "AGVA",
    "NAGVA" = "NAGVA",

    # Expenditure Side (Standard Acronyms)
    "Private Final Consumption Expenditure" = "PFCE",
    "Government Final Consumption Expenditure" = "GFCE",
    "Gross Fixed Capital Formation" = "GFCF",
    "Gross Capital Formation" = "GCF",
    "Change in Stock" = "CIS",
    "Export of Goods and Services" = "Exports",
    "Import of Goods and Services" = "Imports",
    "Net Export" = "Net Exports",
    "Valuables" = "Valuables",

    # Broad Sectors
    "Primary" = "Primary",
    "Secondary" = "Secondary",
    "Tertiary" = "Tertiary",
    "Industry" = "Industry",
    "Services" = "Services",

    # GVA Sectors (Top Level)
    "Agriculture, Livestock, Forestry and Fishing" = "Agriculture & Allied",
    "Agriculture and Allied" = "Agriculture & Allied",
    "Mining and Quarrying" = "Mining & Quarrying",
    "Manufacturing" = "Manufacturing",
    "Electricity, Gas, Water Supply & Other Utility Services" = "Electricity & Utilities",
    "Construction" = "Construction",
    "Trade, Hotels, Transport, Communication & Services Related to Broadcasting" = "Trade, Hotels, Transport & Comm.",
    "Financial, Real Estate & Professional Services" = "Financial, Real Estate & Prof. Services",
    "Public Administration, Defence & Other Services" = "PADO",
    "Public Administration and Defence" = "Public Admin & Defence",

    # GVA Sub-sectors (Agriculture)
    "Crops" = "Crops",
    "Livestock" = "Livestock",
    "Forestry and Logging" = "Forestry",
    "Fishing and Aquaculture" = "Fishing",

    # GVA Sub-sectors (Manufacturing Breakdowns)
    "Food Products, Beverages and Tobacco" = "Food, Beverages & Tobacco",
    "Textiles, Apparel and Leather Products" = "Textiles, Apparel & Leather",
    "Metal Products" = "Metal Products",
    "Machinery and Equipment" = "Machinery & Equipment",
    "Other Manufactured Goods" = "Other Manufacturing",

    # GVA Sub-sectors (Services Breakdowns)
    "Trade, Repair, Hotels and Restaurants" = "Trade & Hotels",
    "Trade & Repair Services" = "Trade & Repair",
    "Hotels & Restaurants" = "Hotels & Restaurants",
    "Transport, Storage, Communication & Services Related to Broadcasting" = "Transport, Storage & Comm.",
    "Railways" = "Railways",
    "Road Transport" = "Road Transport",
    "Water Transport" = "Water Transport",
    "Air Transport" = "Air Transport",
    "Services Incidental to Transport" = "Transport Services",
    "Transport by Other Means" = "Other Transport",
    "Storage" = "Storage",
    "Communication & Services Related to Broadcasting" = "Communication",
    "Financial Services" = "Financial Services",
    "Real Estate, Ownership of Dwelling & Professional Services" = "Real Estate & Prof. Services",
    "Other Services" = "Other Services"
  )

  if(is.null(mapping)) mapping <- gdp_name_mapping

  mapping

}

econ_convert_names <- function(df, name_mapping, only_these_cols = NULL){

  if(is.null(df) || NROW(df) == 0){
    return(df)
  }

  str_cols <- df %>% dplyr::select(where(is.character)) %>% colnames()

  if(!is.null(only_these_cols)){
    str_cols <- intersect(only_these_cols, str_cols)
  }

  if(length(str_cols)==0) return(df)

  for(sc in str_cols){
    this_map <- name_mapping
    this_strs <- df[[sc]]
    this_replace <- unname(name_mapping[this_strs])
    df[[sc]] <- dplyr::if_else(is.na(this_replace), this_strs, this_replace)
  }

  df

}
