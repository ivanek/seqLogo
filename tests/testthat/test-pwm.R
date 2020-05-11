# helper functions ------------------------------------------------------------
test_that("pwm2ic works", {
    expect_warning(pwm2ic(dfDNA), "must be of class matrix")
    expect_equal(pwm2ic(mDNA), c(1.27807190511264, 1.27807190511264, 1.27807190511264, 
        0.429049405545332, 0.153560655328985, 1.27807190511264, 1.27807190511264, 
        1.27807190511264))
})
test_that("pwm2cons works", {
    expect_warning(pwm2cons(dfDNA), "must be of class matrix")
    expect_equal(pwm2cons(mDNA), "CGCGCGCG")
})

# pwm class -------------------------------------------------------------------
test_that("makePWM works", {
    expect_error(makePWM(NA), "must be a matrix or a dataframe")
    expect_error(makePWM(mDNA, alphabet = "ABC"), "alphabet must be either DNA, RNA or AA")
    expect_error(makePWM(mDNA[1:3, ], alphabet = "DNA"), "DNA motifs must have 4 rows")
    expect_error(makePWM(mDNA[1:3, ], alphabet = "RNA"), "RNA motifs must have 4 rows")
    expect_error(makePWM(mDNA[1:3, ], alphabet = "AA"), "amino acid motifs must have 20 rows")
    expect_warning(makePWM(mDNA + 0.1), "Columns of PWM must add up to 1.0")  # should be an error ?
    expect_s4_class(makePWM(dfDNA), "pwm")
    expect_s4_class(makePWM(mDNA), "pwm")
    ## DNA
    p <- makePWM(mDNA, alphabet = "DNA")
    expect_identical(pwm(p), mDNA)
    expect_identical(consensus(p), "CGCGCGCG")
    expect_identical(p@alphabet, "DNA")
    expect_equal(ic(p), c(1.27807190511264, 1.27807190511264, 1.27807190511264, 0.429049405545332, 
        0.153560655328985, 1.27807190511264, 1.27807190511264, 1.27807190511264))
    ## RNA
    r <- makePWM(mRNA, alphabet = "RNA")
    expect_identical(pwm(r), mRNA)
    expect_identical(consensus(r), "CGCGCGCG")
    expect_identical(r@alphabet, "RNA")
    expect_equal(ic(r), c(1.27807190511264, 1.27807190511264, 1.27807190511264, 0.429049405545332, 
        0.153560655328985, 1.27807190511264, 1.27807190511264, 1.27807190511264))
    ## AA
    aa <- makePWM(mAA, alphabet = "AA")
    expect_identical(pwm(aa), mAA)
    expect_identical(consensus(aa), "CDCDCDCD")
    expect_identical(aa@alphabet, "AA")
    expect_equal(ic(aa), c(1.27807190511264, 1.27807190511264, 1.27807190511264, 
        0.429049405545332, 0.153560655328985, 1.27807190511264, 1.27807190511264, 
        1.27807190511264))
    
})

# functions -------------------------------------------------------------------
test_that("show, summary, plot on pwm-class works", {
    p <- makePWM(mDNA, alphabet = "DNA")
    
    ## show
    out <- paste0("    1   2   3   4   5   6   7   8\\n", "A 0.0 0.0 0.0 0.3 0.2 0.0 0.0 0.0\\n", 
        "C 0.8 0.2 0.8 0.3 0.4 0.2 0.8 0.2\\n", "G 0.2 0.8 0.2 0.4 0.3 0.8 0.2 0.8\\n", 
        "T 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0")
    expect_output(show(p), out)
    
    ## summary
    out <- paste0("Position weight matrix:\\n", "    1   2   3   4   5   6   7   8\\n", 
        "A 0.0 0.0 0.0 0.3 0.2 0.0 0.0 0.0\\n", "C 0.8 0.2 0.8 0.3 0.4 0.2 0.8 0.2\\n", 
        "G 0.2 0.8 0.2 0.4 0.3 0.8 0.2 0.8\\n", "T 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0\\n", 
        "\\n\\nInformation content:\\n", "\\[1\\] 1.2781 1.2781 1.2781 0.4290 0.1536 1.2781 1.2781 1.2781\\n", 
        "\\n\\nConsensus sequence:\\n", "\\[1\\] \"CGCGCGCG\"")
    expect_output(summary(p), out)
    
    ## plot
    expect_null(plot(p))
})
