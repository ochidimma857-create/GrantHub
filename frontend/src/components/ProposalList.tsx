import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { StacksMainnet } from '@stacks/network';
import { callReadOnlyFunction, cvToJSON } from '@stacks/transactions';
import { ProposalCard } from './ProposalCard';
import { CreateProposalForm } from './CreateProposalForm';

interface Proposal {
  id: number;
  proposer: string;
  title: string;
  description: string;
  budget: number;
  milestones: Array<{ description: string; amount: number }>;
  status: string;
  currentMilestone: number;
}

interface ProposalListProps {
  userSession: UserSession;
}

export const ProposalList: React.FC<ProposalListProps> = ({ userSession }) => {
  const [proposals, setProposals] = useState<Proposal[]>([]);
  const [loading, setLoading] = useState(true);
  const [showCreateForm, setShowCreateForm] = useState(false);

  const network = new StacksMainnet();
  const contractAddress = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'; // Replace with actual contract address
  const contractName = 'granthubcontract';

  useEffect(() => {
    loadProposals();
  }, []);

  const loadProposals = async () => {
    try {
      setLoading(true);
      const proposalCountResult = await callReadOnlyFunction({
        network,
        contractAddress,
        contractName,
        functionName: 'get-proposal-count',
        functionArgs: [],
        senderAddress: userSession.loadUserData().profile.stxAddress.mainnet,
      });

      const count = cvToJSON(proposalCountResult).value.value;
      const proposalPromises = [];

      for (let i = 1; i <= count; i++) {
        proposalPromises.push(
          callReadOnlyFunction({
            network,
            contractAddress,
            contractName,
            functionName: 'get-proposal',
            functionArgs: [cvToJSON({ value: i })],
            senderAddress: userSession.loadUserData().profile.stxAddress.mainnet,
          })
        );
      }

      const proposalResults = await Promise.all(proposalPromises);
      const loadedProposals = proposalResults.map((result, index) => ({
        id: index + 1,
        ...cvToJSON(result).value.value,
      }));

      setProposals(loadedProposals);
    } catch (error) {
      console.error('Error loading proposals:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleProposalCreated = () => {
    setShowCreateForm(false);
    loadProposals(); // Reload proposals after creating new one
  };

  if (loading) {
    return <div style={{ textAlign: 'center', padding: '2rem' }}>Loading proposals...</div>;
  }

  return (
    <div>
      <div style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        marginBottom: '2rem'
      }}>
        <h2 style={{ color: '#64ffda' }}>Grant Proposals</h2>
        <button
          onClick={() => setShowCreateForm(!showCreateForm)}
          style={{
            background: showCreateForm ? '#f44336' : '#64ffda',
            color: '#1a1a1a',
            border: 'none',
            padding: '0.75rem 1.5rem',
            borderRadius: '8px',
            fontWeight: 'bold',
            cursor: 'pointer'
          }}
        >
          {showCreateForm ? 'Cancel' : 'Create Proposal'}
        </button>
      </div>

      {showCreateForm && (
        <CreateProposalForm
          userSession={userSession}
          onProposalCreated={handleProposalCreated}
        />
      )}

      <div>
        {proposals.length === 0 ? (
          <div style={{
            textAlign: 'center',
            padding: '3rem',
            background: '#2a2a2a',
            borderRadius: '12px',
            color: '#ccc'
          }}>
            <h3>No proposals yet</h3>
            <p>Be the first to submit a grant proposal!</p>
          </div>
        ) : (
          proposals.map((proposal) => (
            <ProposalCard
              key={proposal.id}
              proposal={proposal}
              userSession={userSession}
              onVote={() => loadProposals()}
            />
          ))
        )}
      </div>
    </div>
  );
};
