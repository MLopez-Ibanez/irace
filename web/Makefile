HOST=iridia
REMOTEDIR=/var/www/iridia.ulb.ac.be/irace/
RSYNC_FLAGS =  -rlczCI --copy-links --stats --progress  \
--exclude=.svn --exclude=.checksum-ignore 

default:
	@echo "make [ get | put | testget | testput ]"

testput: files/README.html
	rsync $(RSYNC_FLAGS) --dry-run --delete --delete-excluded . $(HOST):$(REMOTEDIR)

testget:
	rsync $(RSYNC_FLAGS) --dry-run $(HOST):$(REMOTEDIR) .

get:
	rsync $(RSYNC_FLAGS) $(HOST):$(REMOTEDIR) .

put: files/README.html
	rsync $(RSYNC_FLAGS)  --delete --delete-excluded . $(HOST):$(REMOTEDIR)

files/README.html : ../README.md
	pandoc -s --toc --toc-depth=4  --ascii ../README.md > files/README.html

