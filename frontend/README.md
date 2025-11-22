# GrantHub DAO Frontend

A modern React frontend for the GrantHub DAO built with Stack.js for blockchain interaction.

## Features

- **Wallet Integration**: Connect Stacks wallets using @stacks/connect
- **Proposal Management**: View, create, and vote on grant proposals
- **Milestone Tracking**: Monitor proposal progress and milestone completion
- **Real-time Updates**: Live updates of proposal status and voting results

## Tech Stack

- **React 18** - UI framework
- **TypeScript** - Type safety
- **Stack.js** - Stacks blockchain integration
- **Vite** - Build tool and dev server
- **pnpm** - Package management

## Getting Started

1. Install dependencies:
   ```bash
   pnpm install
   ```

2. Start development server:
   ```bash
   pnpm dev
   ```

3. Open [http://localhost:3000](http://localhost:3000) in your browser

## Building for Production

```bash
pnpm build
```

## Configuration

Update the contract address in the components to match your deployed GrantHub contract:

```typescript
const contractAddress = 'YOUR_CONTRACT_ADDRESS_HERE';
```

## Features Overview

### Wallet Connection
- Secure wallet connection using Stacks Connect
- Automatic session management
- User-friendly connection flow

### Proposal System
- Create detailed grant proposals with milestones
- Vote on proposals using governance tokens
- Real-time voting results and status updates
- Milestone-based fund releases

### User Experience
- Responsive design for all devices
- Dark theme optimized for blockchain applications
- Intuitive forms and interactions
- Loading states and error handling
