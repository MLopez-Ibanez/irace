HOST=iridia
REMOTEDIR=/home/httpd/html/irace/
RSYNC_FLAGS =  -rlczCI --copy-links --stats --progress  \
--exclude=.svn --exclude=.checksum-ignore 

default:
	@echo "make [ get | put | testget | testput ]"

testput: 
	rsync $(RSYNC_FLAGS) --dry-run --delete --delete-excluded . $(HOST):$(REMOTEDIR)

testget:
	rsync $(RSYNC_FLAGS) --dry-run $(HOST):$(REMOTEDIR) .

get:
	rsync $(RSYNC_FLAGS) $(HOST):$(REMOTEDIR) .

put: 
	rsync $(RSYNC_FLAGS)  --delete --delete-excluded . $(HOST):$(REMOTEDIR)
