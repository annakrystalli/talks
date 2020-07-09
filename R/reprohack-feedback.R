library(ggplot2)
library(ggwordcloud)
library(ggtext)
rh_diff_wordcloud <- function(feedback_long, max_n = 60, 
                              filter = rep(TRUE, nrow(feedback_long)),
                              question = c("repro_positives",
                                           "repro_challenges"),
                              max_size = 9,
                              label_pos = "positive",
                              label_neg = "challenging",
                              background_color = "#353a3f",
                              neg_color = "#f25a22",
                              pos_color = "#03CC99",
                              title_size = 10) {
    
    checkmate::assert_subset(question, choices = unique(feedback_long$question))
    checkmate::assert_true(length(question) == 2)
    
    pos_var <- question[1]
    neg_var <- question[2]
    
    
    
    data <- feedback_long %>%
        dplyr::filter(filter) %>%
        dplyr::group_by(question,  word) %>%
        dplyr::summarise(freq = dplyr::n()) %>%
        split(f = .$question) %>%
        purrr::map_df(~dplyr::arrange(.x, dplyr::desc(.data$freq)) %>%
                          dplyr::filter(.data$question %in% .env$question) %>%
                          head(max_n),
                      question) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$word, .data$freq, .data$question) %>%
        tidyr::pivot_wider(names_from = question, values_from = freq) %>%
        dplyr::mutate( neg_var = .data[[neg_var]],
                       pos_var  = .data[[pos_var]]) %>% 
        tidyr::replace_na(replace = list(word = NA, neg_var = 0, pos_var = 0)) %>%
        dplyr::mutate( neg_var = prop.table(.data$neg_var)  * 100,
                       pos_var  = prop.table(.data$pos_var) * 100, 
                       repro_diff = .data$pos_var - .data$neg_var,
        ) %>%  
        dplyr::filter(!.data$pos_var == .data$neg_var) %>%
        dplyr::mutate(col = dplyr::case_when(
            repro_diff > 0 ~ "positive",
            repro_diff < 0 ~ "negative"),
            angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE,
                                prob = c(60, 40))) 
    
    data %>%  
        ggplot(aes(label = word, size = abs(repro_diff) * 1000, 
                   angle = angle,colour = repro_diff)) +
        geom_text_wordcloud(area_corr = TRUE, 
                            grid_margin = 0.7, 
                            grid_size = 1) +
        scale_size_area(max_size = max_size) +
        theme_minimal()  %+replace% 
        theme(
            plot.title = element_markdown(lineheight = 1.1,
                                          color="white", size = title_size),
            #element_text( face="bold.italic"),
            plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color, colour = NA)) +
        scale_colour_gradient2(low = neg_color,
                               high = pos_color) +
        ggtitle(glue::glue("Difference in relative frequency of the top {max_n} terms \n used when talking about {label_pos} and {label_neg} ")) +
        labs(
            title = glue::glue("Difference in relative frequency of the top {max_n} terms \n used when talking about    
    <span style='color:{pos_color};'> {label_pos}</span> and
    <span style='color:{neg_color};'> {label_neg} </span> aspects of approaches to reproducibility.
    </span>")
        ) 
    
} 

rh_diff_byscore_wordcloud <- function(feedback_long, max_n = 60, 
                                      comp_window = 0.3,
                                      question = "repro_proceedure",
                                      score = "repro_score",
                                max_size = 9,
                                label_pos = "upper",
                                label_neg = "lower",
                                background_color = "#353a3f",
                                neg_color = "#f25a22",
                                pos_color = "#03CC99",
                                title_size = 10,
                                text_font = NULL,
                                question_label = "describing paper approach to reproducibility") {

    
    pos_var <- "upper"
    neg_var <- "lower"
    
    
    checkmate::assert_number(comp_window, lower = 0, upper = 1)
    up_min <- 10 * (1 - comp_window)
    low_max <- 10 *  comp_window
    
    feedback_long %>%
        dplyr::filter(.data$question == .env$question) %>%
        dplyr::filter(.data[[score]]  >= .env$up_min |
                          .data[[score]]  < .env$low_max   ) %>%
        dplyr::mutate(group = 
                          dplyr::case_when(.data[[score]]  >= .env$up_min ~ "upper",
                                           .data[[score]]  < .env$low_max ~ "lower")) %>%
        dplyr::group_by(group,  word) %>%
        dplyr::summarise(freq = dplyr::n()) %>%
        split(f = .$group) %>%
        purrr::map_df(~dplyr::mutate(.x, 
                                     rel_freq = prop.table(.data$freq)) %>%
                          dplyr::arrange(dplyr::desc(.data$rel_freq)) %>%
                          head(max_n)) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$word, .data$rel_freq, .data$group) %>%
        tidyr::pivot_wider(names_from = group, values_from = rel_freq)  %>% 
        tidyr::replace_na(replace = list(word = NA, lower = 0, upper = 0)) %>%
        dplyr::mutate(repro_diff = .data$upper - .data$lower) %>%  
        dplyr::filter(!.data$upper == .data$lower) %>%
        dplyr::mutate(
            angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE,
                                prob = c(60, 40)))  %>%  
        ggplot(aes(label = word, size = abs(repro_diff) * 1000, 
                   angle = angle,colour = repro_diff)) +
        geom_text_wordcloud(area_corr = TRUE, 
                            grid_margin = 0.7, 
                            grid_size = 1) +
        scale_size_area(max_size = max_size) +
        theme_minimal()  %+replace% 
        theme(
            plot.title = element_markdown(lineheight = 1.1,
                                          color="white", size = title_size),
            #element_text( face="bold.italic"),
            plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color, colour = NA)) +
        scale_colour_gradient2(low = neg_color,
                               high = pos_color) +
        ggtitle(glue::glue("Difference in relative frequency of the top {max_n} terms \n used when talking about {label_pos} and {label_neg} ")) +
        labs(
            title = glue::glue("Difference in relative frequency of top {max_n} terms \n used when     
    <span style='color:{pos_color};'> {label_pos}</span> and
    <span style='color:{neg_color};'> {label_neg} </span>
    </span>")
        ) 
    
} 


rh_wordcloud <- function(feedback_long, max_n = 60, 
                         filter = rep(TRUE, nrow(feedback_long)),
                         question = c("repro_positives",
                                      "repro_challenges"),
                         max_size = 10,
                         label_pos = "positives",
                         label_neg = "challenges",
                         background_color = "#353a3f",
                         neg_color = "#f25a22",
                         pos_color = "#03CC99",
                         title_size = 10) {
    
    feedback_long %>%
        dplyr::filter(filter) %>%
        dplyr::group_by(question,  word) %>%
        dplyr::summarise(freq = dplyr::n()) %>%
        split(f = .$question) %>%
        purrr::map_df(~dplyr::arrange(.x, dplyr::desc(.data$freq)) %>%
                          dplyr::filter(.data$question %in% .env$question) %>%
                          head(max_n),
                      question) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$word, .data$freq, .data$question) %>%
        dplyr::mutate(angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(60, 40))) %>%  
        #ggwordcloud::ggwordcloud2()
        
        ggplot(aes(label = word, size = freq * 1000, angle = angle, 
                   x = question, colour = question)) +
        geom_text_wordcloud(area_corr = TRUE, grid_margin = 0.7, grid_size = 1) +
        scale_size_area(max_size = max_size) +
        theme_minimal() %+replace% 
        theme(
            plot.title = element_markdown(lineheight = 1.1,
                                          color="white", size = title_size),
            #element_text( face="bold.italic"),
            plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color, colour = NA)) +
        scale_colour_manual(values = c(neg_color, pos_color)) +
        labs(
            title = glue::glue("Relative frequency of the top {max_n} terms \n used when talking about    
    <span style='color:{pos_color};'> {label_pos}</span> and
    <span style='color:{neg_color};'> {label_neg} </span>
    </span>")
        ) 
    
    
}

rh_wordcloud_comp_score <- function(feedback_long, max_n = 60, 
                         comp_window = 0.3,
                         question = "repro_proceedure",
                         score = "repro_score",
                         max_size = 10,
                         label_pos = "upper",
                         label_neg = "lower",
                         background_color = "#353a3f",
                         neg_color = "#f25a22",
                         pos_color = "#03CC99",
                         text_font = NULL,
                         title_size = 10,
                         question_label = "describing paper approach to reproducibility") {
    
    checkmate::assert_number(comp_window, lower = 0, upper = 1)
    up_min <- 10 * (1 - comp_window)
    low_max <- 10 *  comp_window
    
    feedback_long %>%
        dplyr::filter(.data$question == .env$question) %>%
        dplyr::filter(.data[[score]]  >= .env$up_min |
                          .data[[score]]  < .env$low_max   ) %>%
        dplyr::mutate(group = 
            dplyr::case_when(.data[[score]]  >= .env$up_min ~ "upper",
                             .data[[score]]  < .env$low_max ~ "lower")) %>%
        dplyr::group_by(group,  word) %>%
        dplyr::summarise(freq = dplyr::n()) %>%
        split(f = .$group) %>%
        purrr::map_df(~dplyr::mutate(.x, 
                                     rel_freq = prop.table(.data$freq)) %>%
                          dplyr::arrange(dplyr::desc(.data$rel_freq)) %>%
                          head(max_n),
                      question) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$word, .data$rel_freq, .data$group) %>%
        dplyr::mutate(angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(60, 40))) %>%  
        #ggwordcloud::ggwordcloud2()
        
        ggplot(aes(label = word, size = rel_freq, angle = angle, 
                   x = group, colour = group)) +
        geom_text_wordcloud(area_corr = TRUE, grid_margin = 0.7, grid_size = 1) +
        scale_size_area(max_size = max_size) +
        theme_minimal() %+replace% 
        theme(
            panel.grid = element_blank(),
            plot.title = element_markdown(lineheight = 1.1,
                                          color="white", size = title_size),
            text = element_text(
                family = text_font,
                colour = "white"),
            plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color, colour = NA)) +
        scale_colour_manual(values = c(neg_color, pos_color)) +
        labs(
            title = glue::glue("Relative frequency of the top {max_n} terms \n used when {question_label},
            subset according to {score}:
    <span style='color:{pos_color};'> {label_pos} ({up_min}-10) </span> and
    <span style='color:{neg_color};'> {label_neg} (0-{low_max}) </span>
    </span>")
        ) 
    
}

#' Merge paper submission & feedback gsheets
#'
#' @param submission_form_title Gform Paper Submission form title
#' @param feedback_form_title Gform Feedback form title
#' @param match_length length of title in characters to be used for fuzzy matching
#' @param gargle_oauth_cache path to token cache
#' @param gargle_oauth_email email address corresponding to your preferred Google identity
#'
#' @return tibble of
#' @export
#'
#' @examples
feedback_join_papers <- function(submission_form_title = "ReproHack Paper Submission Form - n8",
                                 feedback_form_title = "ReproHack feedback form - n8",
                                 match_length = 30,
                                 gargle_oauth_cache = ".secrets",
                                 gargle_oauth_email = "annakrystalli@googlemail.com",
                                 remove_private_fields = FALSE) {
    
    options(
        gargle_oauth_cache = gargle_oauth_cache,
        gargle_oauth_email = gargle_oauth_email
    )
    
    papers <- googlesheets4::sheets_read(
        googlesheets4::sheets_find(submission_form_title)) %>%
        janitor::clean_names() %>%
        dplyr::mutate(match_name = stringr::str_trunc(
            .data$paper_title,
            .env$match_length
        )) %>%
        dplyr::rename(timestamp_paper = timestamp) %>%
        dplyr::select(paper_title, match_name, name, 
                      email_address, timestamp_paper)
    
    # load feedback
    feedback <- googlesheets4::sheets_read(
        googlesheets4::sheets_find(feedback_form_title)
    ) %>% janitor::clean_names() %>%
        dplyr::mutate(
            match_name =
                stringr::str_trunc(
                    .data$which_paper_did_you_attempt,
                    .env$match_length
                )
        ) %>%
        dplyr::rename(timestamp_feedback = timestamp)
    
    out <- fuzzyjoin::stringdist_left_join(
        feedback, papers, by = c(match_name = "match_name"),
        max_dist = 10, method = "qgram",
        # method = "cosine",
        ignore_case = TRUE
    ) %>%
        dplyr::select(
            .data$paper_title,
            .data$which_paper_did_you_attempt, 
            .data$name,
            .data$email_address,
            .data$timestamp_paper,
            .data$name_of_participant_s,
            .data$contact_email,
            .data$timestamp_feedback,
            dplyr::everything()
        ) %>%
        dplyr::arrange(.data$timestamp_feedback) %>%
        dplyr::select(-dplyr::starts_with("match_name"),
                      - .data$attach_document_with_additional_review_comments) %>%
        assertr::verify(!is.na(.data$which_paper_did_you_attempt)) 

    if(remove_private_fields){
        out %>%
            dplyr::select(-dplyr::contains("name")) %>%
            dplyr::select(-dplyr::contains("email"))
    }else{out}
    
    }


papers_get_summaries <- function(papers_joined, remove_private_fields = FALSE) {

    if(remove_private_fields) {
        dplyr::group_by(papers_joined, .data$paper_title) %>%
        dplyr::summarise(
            no_attempts = dplyr::n(),
            mean_repro = mean(.data$on_a_scale_of_1_to_10_how_much_of_the_paper_did_you_manage_to_reproduce) %>%
                round()
        )
    }else{
        dplyr::group_by(papers_joined, .data$paper_title) %>%
            dplyr::summarise(
                no_attempts = dplyr::n(),
                mean_repro = mean(.data$on_a_scale_of_1_to_10_how_much_of_the_paper_did_you_manage_to_reproduce) %>%
                    round(),
                name = unique(.data$name),
                email_address = unique(.data$email_address)
            )
    }
}

authors_filter <- function(papers_joined){
    papers_joined %>%
        dplyr::filter(
            .data$would_you_like_to_receive_a_copy_of_any_feedback_on_the_paper == "Yes")
}

#' @param feedback_form_title Gform Feedback form title
#' @param match_length length of title in characters to be used for fuzzy matching
#' @param gargle_oauth_cache path to token cache
#' @param gargle_oauth_email email address corresponding to your preferred Google identity


feedback_get_questions <- function(feedback_form_title = "ReproHack feedback form - n8",
                                   gargle_oauth_cache = ".secrets",
                                   gargle_oauth_email = "annakrystalli@googlemail.com") {
    options(
        gargle_oauth_cache = gargle_oauth_cache,
        gargle_oauth_email = gargle_oauth_email
    )
    
    questions <- googlesheets4::sheets_read(
        googlesheets4::sheets_find(feedback_form_title)
    ) %>% names()
    
    names(questions) <- questions %>%
        janitor::make_clean_names()
    
    names(questions)[names(questions) == "timestamp"] <- "timestamp_feedback"
    
    questions
}


email_merge <- function(template_path = "inst/templates/email.Rmd", 
                        out_filename_prefix = "email_merged",
                        out_dir = "emails",
                        data = list()) {
    
    template_contents <- strsplit(
        whisker::whisker.render(
            xfun::read_utf8(template_path), data), 
        "\n")[[1]]
    
    
    file_name <- janitor::make_clean_names(data$name)
    
    new <- usethis::write_over(
        file.path(out_dir,
                  glue::glue("{out_filename_prefix}_{file_name}.Rmd")), 
        template_contents)
    
    invisible(new)
}

email_feedback <- function(paper, feedback, questions,
                           template_path = "inst/templates/email.Rmd",
                           subject = "[reprohack-feedback] Feedback from the N8 CIR ReproHack Northern Tour Series",
                           from = setNames("reprohack.team@gmail.com", "ReproHack Core Team")) {
    
    email <- blastula::render_email(template_path,
                                    render_options = list(
                                        params = list(
                                            feedback = feedback,
                                            questions = questions,
                                            name = paper$name,
                                            paper_title = paper$paper_title,
                                            no_attempts = paper$no_attempts,
                                            mean_repro = paper$mean_repro
                                        )))
    
    blastula::smtp_send(
        email = email,
        from = from,
        bcc = from,
        to = paper$email_address,
        #to = "annakrystalli@gmail.com",
        subject = subject,
        credentials = blastula::creds_key(id = "gmail")
    )
}


email_event_feedback <- function(papers, feedback, questions) {
    if(any(duplicated(papers$paper_title))){
        stop("duplicate paper entries in papers")
    }
    split(papers, 1:nrow(papers)) %>%
        purrr::map(~ email_feedback(.x, feedback, questions))
}


recode_feedback <- function(feedback) {
    feedback %>%
        dplyr::rename(repro = .data$did_you_manage_to_reproduce_it,
                      repro_score = .data$on_a_scale_of_1_to_10_how_much_of_the_paper_did_you_manage_to_reproduce,
                      repro_proceedure = .data$briefly_describe_the_procedure_followed_tools_used_to_reproduce_it,
                      repro_familiarity = .data$briefly_describe_your_familiarity_with_the_procedure_tools_used_by_the_paper,
                      os = .data$what_operating_system_were_you_working_on_what_additional_software_did_you_need_to_install,
                      repro_challenges = .data$what_were_the_main_challenges_you_ran_into_if_any,
                      repro_positives = .data$what_were_the_positive_features_of_this_approach,
                      repro_comments = .data$any_other_comments_suggestions_on_the_reproducibility_approach,
                      docs_score = .data$how_well_was_the_material_documented,
                      docs_improve = .data$how_could_the_documentation_be_improved,
                      docs_enjoy = .data$what_did_you_like_about_the_documentation,
                      transparency_score = .data$after_attempting_to_reproduce_how_familiar_do_you_feel_with_code_and_method_used_in_the_paper,
                      transparency_improve = .data$any_suggestions_on_how_the_analysis_could_be_made_more_transparent,
                      reuse_score = .data$rate_the_project_on_reusability_of_the_material,
                      permissive_license = .data$are_materials_clearly_covered_by_a_permissive_enough_license_to_build_on,
                      reuse_improve = .data$any_suggestions_on_how_the_project_could_be_more_reusable,
                      comments = .data$any_final_comments)  
}


