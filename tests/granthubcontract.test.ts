import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const address1 = accounts.get("wallet_1")!;

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
});