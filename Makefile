sync:
	rsync -e ssh -vaurz --delete . ssh.tardis.ed.ac.uk:/var/autofs/www/wwaites/pages/
