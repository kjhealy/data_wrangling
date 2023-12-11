## Data Wrangling deploy
## Requires if (Sys.info()["user"] == "kjhealy") to be TRUE in _targets.R
PUBLIC_DIR='_site/'
SSH_USER='kjhealy@kjhealy.co'
DOCUMENT_ROOT='~/public/kjhealy/public_html/dw'

echo "Uploading changes to remote server..."
cp html/htaccess $PUBLIC_DIR/.htaccess
rsync --exclude='.DS_Store' -Prvzce 'ssh -p 22' $PUBLIC_DIR $SSH_USER:$DOCUMENT_ROOT --delete-after
