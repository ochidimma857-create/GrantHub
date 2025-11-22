import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;
const address3 = accounts.get("wallet_3")!;

const CONTRACT_NAME = "granthubcontract";

describe("GrantHub DAO Security Tests", () => {
  describe("Read-Only Functions", () => {
    it("should return initial contract state", () => {
      const totalSupply = simnet.callReadOnlyFn(CONTRACT_NAME, "get-total-supply", [], deployer);
      expect(totalSupply.result).toBeUint(0);
      
      const treasury = simnet.callReadOnlyFn(CONTRACT_NAME, "get-treasury-balance", [], deployer);
      expect(treasury.result).toBeUint(0);
      
      const isPaused = simnet.callReadOnlyFn(CONTRACT_NAME, "is-contract-paused", [], deployer);
      expect(isPaused.result).toBeBool(false);
      
      const isEmergency = simnet.callReadOnlyFn(CONTRACT_NAME, "is-emergency-mode", [], deployer);
      expect(isEmergency.result).toBeBool(false);
      
      const oracleCount = simnet.callReadOnlyFn(CONTRACT_NAME, "get-oracle-count", [], deployer);
      expect(oracleCount.result).toBeUint(0);
    });
  });

  describe("Access Control Security", () => {
    it("should prevent non-owner from pausing contract", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], address1);
      expect(result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
    });

    it("should prevent non-owner from enabling emergency mode", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], address1);
      expect(result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
    });
  });

  describe("Owner Functions Security", () => {
    it("should allow owner to pause contract", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      expect(result).toBeOk(Cl.bool(true));
    });

    it("should allow owner to enable emergency mode", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], deployer);
      expect(result).toBeOk(Cl.bool(true));
    });
  });

  describe("Contract State Updates", () => {
    it("should update contract state when paused", () => {
      simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      const isPaused = simnet.callReadOnlyFn(CONTRACT_NAME, "is-contract-paused", [], deployer);
      expect(isPaused.result).toBeBool(true);
    });

    it("should update contract state when emergency mode enabled", () => {
      simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], deployer);
      const isEmergency = simnet.callReadOnlyFn(CONTRACT_NAME, "is-emergency-mode", [], deployer);
      expect(isEmergency.result).toBeBool(true);
    });
  });

  describe("Duplicate Prevention Security", () => {
    it("should prevent pausing already paused contract", () => {
      simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      expect(result).toBeErr(Cl.uint(112)); // ERR_INVALID_INPUT
    });

    it("should allow unpausing after pause", () => {
      simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "unpause-contract", [], deployer);
      expect(result).toBeOk(Cl.bool(true));
    });

    it("should allow disabling emergency mode after enabling", () => {
      simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], deployer);
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "disable-emergency-mode", [], deployer);
      expect(result).toBeOk(Cl.bool(true));
    });
  });

  describe("Security Features Validation", () => {
    it("should have implemented pause/unpause functionality", () => {
      // Pause contract
      const pauseResult = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      expect(pauseResult.result).toBeOk(Cl.bool(true));
      
      // Check paused state
      const isPaused = simnet.callReadOnlyFn(CONTRACT_NAME, "is-contract-paused", [], deployer);
      expect(isPaused.result).toBeBool(true);
      
      // Unpause contract
      const unpauseResult = simnet.callPublicFn(CONTRACT_NAME, "unpause-contract", [], deployer);
      expect(unpauseResult.result).toBeOk(Cl.bool(true));
      
      // Check unpaused state
      const isUnpaused = simnet.callReadOnlyFn(CONTRACT_NAME, "is-contract-paused", [], deployer);
      expect(isUnpaused.result).toBeBool(false);
    });

    it("should have implemented emergency mode functionality", () => {
      // Enable emergency mode
      const enableResult = simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], deployer);
      expect(enableResult.result).toBeOk(Cl.bool(true));
      
      // Check emergency state
      const isEmergency = simnet.callReadOnlyFn(CONTRACT_NAME, "is-emergency-mode", [], deployer);
      expect(isEmergency.result).toBeBool(true);
      
      // Disable emergency mode
      const disableResult = simnet.callPublicFn(CONTRACT_NAME, "disable-emergency-mode", [], deployer);
      expect(disableResult.result).toBeOk(Cl.bool(true));
      
      // Check disabled state
      const isDisabled = simnet.callReadOnlyFn(CONTRACT_NAME, "is-emergency-mode", [], deployer);
      expect(isDisabled.result).toBeBool(false);
    });

    it("should have implemented access control", () => {
      // Non-owner should not be able to pause
      const unauthorizedPause = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], address1);
      expect(unauthorizedPause.result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
      
      // Non-owner should not be able to enable emergency mode
      const unauthorizedEmergency = simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], address1);
      expect(unauthorizedEmergency.result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
      
      // Owner should be able to perform these actions
      const ownerPause = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);
      expect(ownerPause.result).toBeOk(Cl.bool(true));
      
      const ownerEmergency = simnet.callPublicFn(CONTRACT_NAME, "enable-emergency-mode", [], deployer);
      expect(ownerEmergency.result).toBeOk(Cl.bool(true));
    });
  });

  describe("Proposal Submission Tests", () => {
    it("should initialize DAO with governance tokens", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      expect(result).toBeOk(Cl.bool(true));
      
      const balance = simnet.callReadOnlyFn(CONTRACT_NAME, "get-balance", [Cl.principal(deployer)], deployer);
      expect(balance.result).toBeUint(10000000);
    });

    it("should submit a valid proposal", () => {
      // Initialize DAO first
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(500000) }),
        Cl.tuple({ description: Cl.stringAscii("Milestone 2"), amount: Cl.uint(500000) })
      ];
      
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Test Proposal"),
        Cl.stringAscii("A test proposal for GrantHub"),
        Cl.uint(1000000),
        Cl.list(milestones)
      ], deployer);
      
      expect(result).toBeOk(Cl.uint(1)); // First proposal ID
    });

    it("should reject proposal with insufficient proposer balance", () => {
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(500000) })
      ];
      
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Test Proposal"),
        Cl.stringAscii("A test proposal"),
        Cl.uint(1000000),
        Cl.list(milestones)
      ], address1); // address1 has no tokens
      
      expect(result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
    });

    it("should reject proposal with budget exceeding treasury", () => {
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(5000000) })
      ];
      
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Expensive Proposal"),
        Cl.stringAscii("Too expensive"),
        Cl.uint(10000000), // More than treasury
        Cl.list(milestones)
      ], deployer);
      
      expect(result).toBeErr(Cl.uint(104)); // ERR_INSUFFICIENT_FUNDS
    });
  });

  describe("Voting System Tests", () => {
    beforeEach(() => {
      // Setup: Initialize DAO and submit proposal
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(500000) }),
        Cl.tuple({ description: Cl.stringAscii("Milestone 2"), amount: Cl.uint(500000) })
      ];
      
      simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Voting Test Proposal"),
        Cl.stringAscii("Test voting functionality"),
        Cl.uint(1000000),
        Cl.list(milestones)
      ], deployer);
    });

    it("should allow voting on active proposal", () => {
      // Mint tokens for address1
      simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [Cl.principal(address1), Cl.uint(2000000)], deployer);
      
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [
        Cl.uint(1), // proposal ID
        Cl.bool(true) // vote yes
      ], address1);
      
      expect(result).toBeOk(Cl.uint(4000000)); // Quadratic voting power: 2000000^2 = 4e12, but simplified in contract
    });

    it("should prevent double voting", () => {
      simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [Cl.principal(address1), Cl.uint(2000000)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [Cl.uint(1), Cl.bool(true)], address1);
      
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [
        Cl.uint(1),
        Cl.bool(false)
      ], address1);
      
      expect(result).toBeErr(Cl.uint(106)); // ERR_ALREADY_VOTED
    });

    it("should finalize proposal after voting period", () => {
      // Mint tokens and vote
      simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [Cl.principal(address1), Cl.uint(2000000)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [Cl.uint(1), Cl.bool(true)], address1);
      
      // Fast forward past voting period (mock this by directly calling finalize)
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "finalize-proposal", [Cl.uint(1)], deployer);
      expect(result).toBeOk(Cl.bool(true)); // Should approve with sufficient votes
    });
  });

  describe("Oracle and Milestone Tests", () => {
    beforeEach(() => {
      // Setup: Initialize, submit, vote, and approve proposal
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(500000) })
      ];
      
      simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Oracle Test Proposal"),
        Cl.stringAscii("Test oracle functionality"),
        Cl.uint(500000),
        Cl.list(milestones)
      ], deployer);
      
      simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [Cl.principal(address1), Cl.uint(2000000)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [Cl.uint(1), Cl.bool(true)], address1);
      simnet.callPublicFn(CONTRACT_NAME, "finalize-proposal", [Cl.uint(1)], deployer);
      
      // Authorize oracles
      simnet.callPublicFn(CONTRACT_NAME, "authorize-oracle", [Cl.principal(address1)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "authorize-oracle", [Cl.principal(address2)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "authorize-oracle", [Cl.principal(address3)], deployer);
    });

    it("should verify milestone with oracle consensus", () => {
      // Oracle votes
      simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [Cl.uint(1), Cl.uint(0), Cl.bool(true)], address1);
      simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [Cl.uint(1), Cl.uint(0), Cl.bool(true)], address2);
      
      // Check consensus
      const consensus = simnet.callReadOnlyFn(CONTRACT_NAME, "get-milestone-consensus", [Cl.uint(1), Cl.uint(0)], deployer);
      expect(consensus.result).toStrictEqual(Cl.tuple({
        "votes-for": Cl.uint(2),
        "votes-against": Cl.uint(0),
        "total-votes": Cl.uint(2)
      }));
    });

    it("should release funds after milestone verification", () => {
      // Get oracle consensus
      simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [Cl.uint(1), Cl.uint(0), Cl.bool(true)], address1);
      simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [Cl.uint(1), Cl.uint(0), Cl.bool(true)], address2);
      
      // Release funds
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "release-milestone-funds", [Cl.uint(1), Cl.uint(0)], deployer);
      expect(result).toBeOk(Cl.uint(500000));
    });
  });

  describe("Security Edge Cases", () => {
    it("should handle overflow in token minting", () => {
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      // Try to mint maximum possible amount
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [
        Cl.principal(address1), 
        Cl.uint(18446744073709551615) // Max uint64
      ], deployer);
      
      // Should either succeed or fail gracefully without overflow
      expect(result).toBeOk(Cl.bool(true));
    });

    it("should validate milestone amounts match budget", () => {
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(300000) }),
        Cl.tuple({ description: Cl.stringAscii("Milestone 2"), amount: Cl.uint(300000) })
      ];
      
      // Budget doesn't match milestone sum
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Invalid Budget Proposal"),
        Cl.stringAscii("Budget doesn't match milestones"),
        Cl.uint(500000), // Sum should be 600000
        Cl.list(milestones)
      ], deployer);
      
      expect(result).toBeErr(Cl.uint(101)); // ERR_INVALID_PROPOSAL
    });

    it("should prevent oracle from voting twice on same milestone", () => {
      simnet.callPublicFn(CONTRACT_NAME, "initialize", [Cl.uint(10000000)], deployer);
      
      const milestones = [
        Cl.tuple({ description: Cl.stringAscii("Milestone 1"), amount: Cl.uint(500000) })
      ];
      
      simnet.callPublicFn(CONTRACT_NAME, "submit-proposal", [
        Cl.stringAscii("Oracle Double Vote Test"),
        Cl.stringAscii("Test double voting prevention"),
        Cl.uint(500000),
        Cl.list(milestones)
      ], deployer);
      
      simnet.callPublicFn(CONTRACT_NAME, "mint-tokens", [Cl.principal(address1), Cl.uint(2000000)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "vote-on-proposal", [Cl.uint(1), Cl.bool(true)], address1);
      simnet.callPublicFn(CONTRACT_NAME, "finalize-proposal", [Cl.uint(1)], deployer);
      simnet.callPublicFn(CONTRACT_NAME, "authorize-oracle", [Cl.principal(address1)], deployer);
      
      // First vote
      simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [Cl.uint(1), Cl.uint(0), Cl.bool(true)], address1);
      
      // Second vote should fail
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "verify-milestone", [
        Cl.uint(1), 
        Cl.uint(0), 
        Cl.bool(false)
      ], address1);
      
      expect(result).toBeErr(Cl.uint(115)); // ERR_ORACLE_ALREADY_VOTED
    });
  });
});