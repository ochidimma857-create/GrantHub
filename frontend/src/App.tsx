import React from 'react';
import { Connect } from '@stacks/connect-react';
import { AppConfig, UserSession } from '@stacks/connect';
import { Header } from './components/Header';
import { ProposalList } from './components/ProposalList';
import { WalletConnect } from './components/WalletConnect';
import './App.css';

const appConfig = new AppConfig(['store_write', 'publish_data']);
const userSession = new UserSession({ appConfig });

function App() {
  return (
    <Connect
      authOptions={{
        appDetails: {
          name: 'GrantHub DAO',
          icon: window.location.origin + '/logo.png',
        },
        redirectTo: '/',
        onFinish: () => {
          window.location.reload();
        },
        userSession,
      }}
    >
      <div className="App">
        <Header />
        <main>
          {!userSession.isUserSignedIn() ? (
            <WalletConnect />
          ) : (
            <ProposalList userSession={userSession} />
          )}
        </main>
      </div>
    </Connect>
  );
}

export default App;
