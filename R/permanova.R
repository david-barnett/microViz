
permanova <- function(ps,
                     variables,
                     interactions = NULL,
                     n_processes = 2,
                     n_perms = 999,
                     seed = 1,
                     adonis2 = FALSE
){

  #  aggregate taxa if requested
  if (!is.null(tax_level) && tax_level != 'given'){
    if(length(Dist) == 1 && grepl('unifrac', Dist)){stop("Don't aggregate taxa with unifrac metrics!")}
    ps = microbiome::aggregate_taxa(ps, level = tax_level)
  }

  # set seed for reproducibility
  set.seed(seed)

  # Build a formula
  f = paste0('distMatrix ~ ', paste(Variables, collapse = ' + '))
  if (!is.null(Interactions)) { f = paste0(f, ' + ', paste(Interactions, collapse = ' + ')) }
  FORMULA = as.formula(f)

  # handle missings in metadata
  ps = handle_missings(ps, Vars = Variables, completeCases = complete_cases)

  # extract sample metadata from phyloseq object
  metadata = microbiome::meta(ps)[, Variables, drop = FALSE]

  # calculate distances (if a distance matrix is not already given)
  if (class(Dist) != 'dist') {

    # special case aitchison distance: clr transform and calculate euclidean distances
    if (Dist == 'aitchison') {
      ps = microbiome::transform(ps, transform = 'clr')
      Dist = 'euclidean'
    }

    message(Sys.time(), ' - Calculating ', Dist, ' distance')
    distMatrix = my_distance(pSeq = ps, DIST = Dist)

  } else if (class(Dist) == 'dist'){
    distMatrix = Dist
  }


  if (isTRUE(betadisper) || is.character(betadisper) && betadisper %in% c("centroid", "median")){

    # betadisper --------------------------------------------------------------

    # if betadisper method not chosen, use centroid method
    if (isTRUE(betadisper)){
      betadisper = 'centroid'
      message('Note: Automatically choosing centroid method (not median)\n\t',
              'Avoid this message by setting betadisper arg to "centroid" or "median"')
    }

    # calculate bdisp and anova and tukeyHSD confidence/significance for all variables
    bdisp = lapply(Variables, function(V){
      if (!class(metadata[[V]]) %in% c('logical', 'character', 'factor', 'integer')){
        warning('Variable ', V, 'is skipped as it cannot be used for grouping (class = ', class(metadata[[V]]), ')')
        return(NULL)
      } else {
        model = vegan::betadisper(d = distMatrix, group = metadata[[V]], type = betadisper)
        Anova = anova(object = model)
        tukeyHSD = stats::TukeyHSD(model)
        return(list(model = model, anova = Anova, tukeyHSD = tukeyHSD))
      }
    })

    names(bdisp) = Variables
    return(bdisp)

  } else {

    # permanova ---------------------------------------------------------------

    message(Sys.time(), ' - Starting PERMANOVA with ', nperms,' perms on ', ncores, ' cores')
    # perform PERMANOVA, in parallel (socket cluster)
    cl <- parallel::makePSOCKcluster(ncores)
    on.exit(parallel::stopCluster(cl))

    if (isTRUE(adonis2)){
      results <- vegan::adonis2(formula = FORMULA, data = metadata, permutations = nperms, parallel = cl, by = 'margin')
    } else {
      results <- vegan::adonis(formula = FORMULA, data = metadata, permutations = nperms, parallel = cl)
    }
    message(Sys.time(), ' - Finished PERMANOVA')

    return(results)

  }
}
