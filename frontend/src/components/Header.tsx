import React from 'react';

export const Header: React.FC = () => {
  return (
    <header style={{
      padding: '1rem 0',
      borderBottom: '1px solid #444',
      marginBottom: '2rem'
    }}>
      <h1 style={{
        margin: 0,
        color: '#64ffda',
        fontSize: '2.5rem',
        fontWeight: 'bold'
      }}>
        GrantHub DAO
      </h1>
      <p style={{
        margin: '0.5rem 0 0 0',
        color: '#ccc',
        fontSize: '1.1rem'
      }}>
        Decentralized Grants Management Platform
      </p>
    </header>
  );
};
