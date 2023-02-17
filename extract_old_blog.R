# examine and extract bits of old blog from wordpress MySQL database
library(tidyverse)

#comments
comments <- read_csv("data/wp_wu4qkd_comments.csv")

comments$comment_author_email %>%
  str_extract("\\.[a-z]{2,3}$") %>%
  str_remove_all("\\.") %>%
  enframe(value = "domain") %>%
  count(domain) %>%
  arrange(desc(n)) %>%
  mutate(domain = as_factor(domain)) %>%
  ggplot(aes(domain,n)) + geom_col()

comments$comment_author_url %>%
  str_extract("\\.[a-z]{2,3}$") %>%
  str_remove_all("\\.") %>%
  enframe(value = "domain") %>%
  count(domain) %>%
  arrange(desc(n)) %>%
  mutate(domain = as_factor(domain)) %>%
  remove_missing() %>%
  ggplot(aes(domain,n)) + geom_col()


posts_raw <- read_csv("data/wp_wu4qkd_posts.csv")

# get unique, latest posts
posts <- posts_raw %>%
  filter(!is.na(post_content)) %>%
  filter(!is.na(post_title)) %>%
  mutate(post_title =  str_to_title(post_title)) %>%
  group_by(post_title) %>%
  filter(post_date == max(post_date)) %>%
  separate(post_name,into=c("post_id","status","rev_num"),remove = FALSE) %>%
  mutate(post_id = ifelse(is.na(post_id),str_sub(guid,start=-3L),post_id)) %>%
  group_by(post_id) %>%
  filter(post_date == max(post_date)) %>%
  select(post_date,post_title,post_content) %>%
  # now clean up the posts
  mutate(post_content = str_remove_all(post_content,"Â")) %>%
  # fix (many) image links
  mutate(post_content = str_replace_all(post_content,
                                        "https://artsteinmetz.com/blog/wp-content/uploads",
                                        "/img"))



yaml_template <-
"---
title: {post_title}
author: Art Steinmetz
date: {as.Date(post_date)}
---
\n"


# add yaml headers to content
posts <- posts %>%
  mutate(yaml = glue::glue(yaml_template)) %>%
  mutate(post_content = paste0(yaml,post_content,collapse = "\n"))

# create save file names
posts <- posts %>%
  mutate(file_name = paste0(as.character(as.Date(post_date)),"_",
                            str_replace_all(post_title,"[^[:alnum:]]+","_"))) %>%
  mutate(file_name = paste0("posts/",trimws(file_name,whitespace = "_"),".qmd"))

# convert to qmd docs
1:nrow(posts) %>%
  walk(\(x)writeLines(posts$post_content[x],posts$file_name[x]))


