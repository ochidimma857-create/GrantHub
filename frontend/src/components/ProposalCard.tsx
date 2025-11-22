import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';
import { StacksMainnet } from '@stacks/network';
import { callReadOnlyFunction, cvToJSON, makeContractCall, broadcastTransaction } from '@stacks/transactions';

interface Milestone {
  description: string;
  amount: number;
}

interface Proposal {
  id: number;
  proposer: string;
  title: string;
  description: string;
  budget: number;
  milestones: Milestone[];
  status: string;
  currentMilestone: number;
}

interface ProposalCardProps {
  proposal: Proposal;
  userSession: UserSession;
  onVote: () => void;
}

export const ProposalCard: React.FC<ProposalCardProps> = ({ proposal, userSession, onVote }) => {
  const [voting, setVoting] = useState(false);

  const network = new StacksMainnet();
  const contractAddress = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'; // Replace with actual contract address
  const contractName = 'granthubcontract';

  const handleVote = async (voteYes: boolean) => {
    try {
      setVoting(true);

      const txOptions = {
        network,
        contractAddress,
        contractName,
        functionName: 'vote-on-proposal',
        functionArgs: [
          cvToJSON({ value: proposal.id }),
          cvToJSON({ value: voteYes })
        ],
        senderKey: userSession.loadUserData().appPrivateKey,
        postConditionMode: 1, // Allow
      };

      const transaction = await makeContractCall(txOptions);
      await broadcastTransaction(transaction, network);

      onVote(); // Refresh proposals
    } catch (error) {
      console.error('Error voting:', error);
      alert('Error voting on proposal');
    } finally {
      setVoting(false);
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'approved': return '#4caf50';
      case 'rejected': return '#f44336';
      case 'completed': return '#2196f3';
      default: return '#ff9800';
    }
  };

  return (
    <div className="proposal-card">
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start' }}>
        <div>
          <h3>{proposal.title}</h3>
          <p style={{ color: '#ccc', fontSize: '0.9rem' }}>
            Proposed by: {proposal.proposer.slice(0, 10)}...
          </p>
        </div>
        <span
          className="status-badge"
          style={{ background: getStatusColor(proposal.status) }}
        >
          {proposal.status}
        </span>
      </div>

      <p style={{ textAlign: 'left', margin: '1rem 0' }}>
        {proposal.description}
      </p>

      <div style={{ textAlign: 'left', margin: '1rem 0' }}>
        <strong>Budget:</strong> {proposal.budget} STX
      </div>

      <div style={{ textAlign: 'left', margin: '1rem 0' }}>
        <strong>Milestones:</strong>
        <ul className="milestone-list">
          {proposal.milestones.map((milestone, index) => (
            <li key={index} className="milestone-item">
              <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                <span>{milestone.description}</span>
                <span style={{ color: '#64ffda', fontWeight: 'bold' }}>
                  {milestone.amount} STX
                </span>
              </div>
              {index === proposal.currentMilestone && proposal.status === 'approved' && (
                <span style={{ color: '#ff9800', fontSize: '0.8rem' }}>
                  Current milestone
                </span>
              )}
            </li>
          ))}
        </ul>
      </div>

      {proposal.status === 'pending' && (
        <div className="vote-buttons">
          <button
            className="btn-primary"
            onClick={() => handleVote(true)}
            disabled={voting}
          >
            {voting ? 'Voting...' : 'Vote Yes'}
          </button>
          <button
            className="btn-secondary"
            onClick={() => handleVote(false)}
            disabled={voting}
          >
            {voting ? 'Voting...' : 'Vote No'}
          </button>
        </div>
      )}
    </div>
  );
};
