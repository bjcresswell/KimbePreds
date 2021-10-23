


# Create gert stamp

# Load gert
library(gert)
library("gitcreds")

# Create commit tag
gert::git_tag_create(name='Draft1', message='Finished first draft of results')

# To merge branches
gert::git_branch_checkout('master') # ensure on master branch
gert::git_merge(ref='Experimental')


gitcreds::gitcreds_get()


gitcreds_set()
