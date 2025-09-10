# Proof of Practice

A study streak verification system that mints NFT badges for consistent learning habits and tracks practice hours across subjects.

## Features

- **Practice Logging**: Log daily study hours by subject
- **Streak Tracking**: Automatic calculation of current and longest streaks
- **NFT Rewards**: Mint achievement NFTs for 7-day, 30-day, and 100-day streaks
- **Verification System**: Optional verification of practice logs by trusted sources

## Contract Functions

### Public Functions
- `log-practice(hours, subject)` - Log daily practice session
- `claim-streak-nft(streak-days, subject)` - Claim NFT for achievement milestones
- `verify-practice-log(user, date)` - Verify practice logs (owner only)

### Read-Only Functions
- `get-practice-log(user, date)` - Get specific practice session details
- `get-user-streaks(user)` - Get user's streak statistics
- `get-nft-achievement(nft-id)` - Get NFT achievement details
- `get-user-nfts(user)` - Get user's achievement NFTs

## Achievement Levels

- **7-Day Streak**: Week Warrior NFT
- **30-Day Streak**: Month Master NFT  
- **100-Day Streak**: Century Scholar NFT

## Usage

1. Log daily practice sessions with hours and subject
2. System automatically tracks streaks and statistics
3. Claim NFT achievements when reaching milestones
4. Display NFT badges as proof of consistent learning