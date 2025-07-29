# medWAS = function(covars, meds, med_info, num_num_prescriptions = 3) {
#
# 	obs_meds = unique(meds$med_name)
#
# 	results = list()
# 	for (this_med in obs_meds) {
#
# 		meds  |>
# 			filter(med_name == this_med, n >= num_num_prescriptions) |>
# 			pull(person_id) ->
# 			pids_w_med
#
# 		covars %>%
# 			mutate(took_med = person_id %in% pids_w_med) ->
# 			to_glm
#
# 		glm(formula = took_med ~ ., data = to_glm) %>%
# 			broom::tidy() %>%
# 			mutate(med = this_med) ->
# 			results[[this_med]]
# 	}
# 	return(bind_rows(results))
# }
