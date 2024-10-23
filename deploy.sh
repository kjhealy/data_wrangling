## Data Wrangling deploy
## Requires if (Sys.info()["user"] == "kjhealy") to be TRUE in _targets.R
PUBLIC_DIR='_site/'
SSH_USER='kjhealy@kjhealy.co'
DOCUMENT_ROOT='~/public/kjhealy/public_html/dw'

echo "Setting permissions ..."
cp html/htaccess $PUBLIC_DIR/.htaccess

find $PUBLIC_DIR -type d -print0 | xargs -0 chmod 755
find $PUBLIC_DIR -type f -print0 | xargs -0 chmod 644

echo "Uploading changes to remote server..."
rsync --exclude='.DS_Store' -Prvzce 'ssh -p 22' $PUBLIC_DIR $SSH_USER:$DOCUMENT_ROOT --delete-after
