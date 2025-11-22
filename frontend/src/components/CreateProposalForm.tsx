import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';
import { StacksMainnet } from '@stacks/network';
import { cvToJSON, makeContractCall, broadcastTransaction, tupleCV, stringAsciiCV, uintCV, listCV } from '@stacks/transactions';

interface Milestone {
  description: string;
  amount: string;
}

interface CreateProposalFormProps {
  userSession: UserSession;
  onProposalCreated: () => void;
}

export const CreateProposalForm: React.FC<CreateProposalFormProps> = ({
  userSession,
  onProposalCreated
}) => {
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [budget, setBudget] = useState('');
  const [milestones, setMilestones] = useState<Milestone[]>([{ description: '', amount: '' }]);
  const [submitting, setSubmitting] = useState(false);

  const network = new StacksMainnet();
  const contractAddress = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'; // Replace with actual contract address
  const contractName = 'granthubcontract';

  const addMilestone = () => {
    if (milestones.length < 10) {
      setMilestones([...milestones, { description: '', amount: '' }]);
    }
  };

  const updateMilestone = (index: number, field: keyof Milestone, value: string) => {
    const updatedMilestones = [...milestones];
    updatedMilestones[index] = { ...updatedMilestones[index], [field]: value };
    setMilestones(updatedMilestones);
  };

  const removeMilestone = (index: number) => {
    if (milestones.length > 1) {
      setMilestones(milestones.filter((_, i) => i !== index));
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    try {
      setSubmitting(true);

      // Validate inputs
      if (!title.trim() || !description.trim() || !budget) {
        alert('Please fill in all required fields');
        return;
      }

      const budgetNum = parseInt(budget);
      if (budgetNum <= 0) {
        alert('Budget must be greater than 0');
        return;
      }

      // Validate milestones
      const validMilestones = milestones.filter(m =>
        m.description.trim() && m.amount && parseInt(m.amount) > 0
      );

      if (validMilestones.length === 0) {
        alert('Please add at least one milestone');
        return;
      }

      const totalMilestoneAmount = validMilestones.reduce((sum, m) => sum + parseInt(m.amount), 0);
      if (totalMilestoneAmount !== budgetNum) {
        alert('Milestone amounts must sum to the total budget');
        return;
      }

      // Prepare milestones for contract call
      const milestoneCVs = validMilestones.map(milestone =>
        tupleCV({
          description: stringAsciiCV(milestone.description),
          amount: uintCV(parseInt(milestone.amount))
        })
      );

      const txOptions = {
        network,
        contractAddress,
        contractName,
        functionName: 'submit-proposal',
        functionArgs: [
          stringAsciiCV(title),
          stringAsciiCV(description),
          uintCV(budgetNum),
          listCV(milestoneCVs)
        ],
        senderKey: userSession.loadUserData().appPrivateKey,
        postConditionMode: 1, // Allow
      };

      const transaction = await makeContractCall(txOptions);
      await broadcastTransaction(transaction, network);

      // Reset form
      setTitle('');
      setDescription('');
      setBudget('');
      setMilestones([{ description: '', amount: '' }]);

      onProposalCreated();
      alert('Proposal submitted successfully!');
    } catch (error) {
      console.error('Error submitting proposal:', error);
      alert('Error submitting proposal. Please try again.');
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="proposal-card" style={{ marginBottom: '2rem' }}>
      <h3 style={{ color: '#64ffda' }}>Create New Proposal</h3>
      <form onSubmit={handleSubmit}>
        <div className="form-group">
          <label htmlFor="title">Title *</label>
          <input
            id="title"
            type="text"
            value={title}
            onChange={(e) => setTitle(e.target.value)}
            placeholder="Enter proposal title"
            maxLength={100}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="description">Description *</label>
          <textarea
            id="description"
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            placeholder="Describe your proposal in detail"
            maxLength={500}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="budget">Total Budget (STX) *</label>
          <input
            id="budget"
            type="number"
            value={budget}
            onChange={(e) => setBudget(e.target.value)}
            placeholder="Enter total budget in STX"
            min="1"
            required
          />
        </div>

        <div className="form-group">
          <label>Milestones</label>
          {milestones.map((milestone, index) => (
            <div key={index} style={{
              display: 'flex',
              gap: '1rem',
              marginBottom: '1rem',
              alignItems: 'flex-end'
            }}>
              <div style={{ flex: 1 }}>
                <input
                  type="text"
                  value={milestone.description}
                  onChange={(e) => updateMilestone(index, 'description', e.target.value)}
                  placeholder={`Milestone ${index + 1} description`}
                  maxLength={200}
                />
              </div>
              <div style={{ width: '150px' }}>
                <input
                  type="number"
                  value={milestone.amount}
                  onChange={(e) => updateMilestone(index, 'amount', e.target.value)}
                  placeholder="Amount (STX)"
                  min="1"
                />
              </div>
              {milestones.length > 1 && (
                <button
                  type="button"
                  onClick={() => removeMilestone(index)}
                  style={{
                    background: '#f44336',
                    color: 'white',
                    border: 'none',
                    padding: '0.5rem',
                    borderRadius: '4px',
                    cursor: 'pointer'
                  }}
                >
                  Remove
                </button>
              )}
            </div>
          ))}

          {milestones.length < 10 && (
            <button
              type="button"
              onClick={addMilestone}
              style={{
                background: '#444',
                color: 'white',
                border: 'none',
                padding: '0.5rem 1rem',
                borderRadius: '4px',
                cursor: 'pointer'
              }}
            >
              Add Milestone
            </button>
          )}
        </div>

        <button
          type="submit"
          disabled={submitting}
          style={{
            background: '#64ffda',
            color: '#1a1a1a',
            border: 'none',
            padding: '1rem 2rem',
            borderRadius: '8px',
            fontSize: '1.1rem',
            fontWeight: 'bold',
            cursor: submitting ? 'not-allowed' : 'pointer',
            width: '100%'
          }}
        >
          {submitting ? 'Submitting...' : 'Submit Proposal'}
        </button>
      </form>
    </div>
  );
};
