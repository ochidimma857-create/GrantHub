import React from 'react';
import { useConnect } from '@stacks/connect-react';

export const WalletConnect: React.FC = () => {
  const { doOpenAuth } = useConnect();

  return (
    <div style={{
      textAlign: 'center',
      padding: '3rem',
      background: '#2a2a2a',
      borderRadius: '12px',
      margin: '2rem auto',
      maxWidth: '500px'
    }}>
      <h2 style={{ color: '#64ffda', marginBottom: '1rem' }}>
        Connect Your Wallet
      </h2>
      <p style={{ color: '#ccc', marginBottom: '2rem' }}>
        Connect your Stacks wallet to participate in GrantHub DAO governance,
        submit proposals, and vote on grants.
      </p>
      <button
        onClick={() => doOpenAuth()}
        style={{
          background: '#64ffda',
          color: '#1a1a1a',
          border: 'none',
          padding: '1rem 2rem',
          borderRadius: '8px',
          fontSize: '1.1rem',
          fontWeight: 'bold',
          cursor: 'pointer',
          transition: 'all 0.2s'
        }}
        onMouseOver={(e) => {
          e.currentTarget.style.background = '#4dd0b4';
        }}
        onMouseOut={(e) => {
          e.currentTarget.style.background = '#64ffda';
        }}
      >
        Connect Wallet
      </button>
    </div>
  );
};
