#!/usr/bin/env fish

set backup_drive /dev/disk/by-label/Backup
set backup_root (mktemp -d)

echo "Mounting $backup_drive to $backup_root"

sudo mount "$backup_drive" "$backup_root" || exit

set last_backup "$backup_root"/last_backup.txt

# Clear last backup file
: > "$last_backup"

function t
    $argv | tee -a "$last_backup"
end

t echo '==== Copying home directory ===='
t rsync -a --delete --progress --stats /home/ryan/ "$backup_root"/homedir \
    --exclude=.ccache \
    --exclude=.local/share/Trash \
    --exclude=.cache/chromium

t echo '==== Copying sda1 (bulk storage) ===='
t rsync -a --delete --progress --stats /mnt/sda1/ "$backup_root"/sda1 \
    --exclude=lost+found \
    --exclude=.Trash-1000

t echo '==== Copying sdb1 (Windows shared data) ===='
t rsync -a --delete --progress --stats /mnt/sdb1/ "$backup_root"/sdb1

t echo '==== Copying boot partition ===='
t rsync -a --delete --progress --stats /boot/ "$backup_root"/boot

t echo '==== Done ===='

echo "Unmounting $backup_root"

sudo umount "$backup_root"
