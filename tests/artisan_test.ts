import {
  Clarinet,
  Tx,
  Chain,
  Account,
  types,
} from "https://deno.land/x/clarinet@v1.0.2/index.ts";
import { assertEquals } from "https://deno.land/std@0.90.0/testing/asserts.ts";

// Constants for testing
const STATUS_PENDING = 0;
const STATUS_COMPLETED = 2;
const STATUS_CANCELLED = 3;
const STATUS_DISPUTED = 4;

Clarinet.test({
  name: "Artisan can register and client can book",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const artisan = accounts.get("wallet_1")!;
    const client = accounts.get("wallet_2")!;

    // Register artisan
    let block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "register-artisan",
        [
          types.utf8("https://example.com/profile"),
          types.utf8("I am a skilled plumber"),
        ],
        artisan.address
      ),
    ]);

    // Check if artisan registration was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), artisan.address);

    // Book artisan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "book-artisan",
        [
          types.principal(artisan.address),
          types.utf8("Need help with plumbing"),
          types.uint(1000),
        ],
        client.address
      ),
    ]);

    // Check if booking was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Get booking details
    const bookingResult = chain.callReadOnlyFn(
      "artisan",
      "get-booking",
      [types.uint(1)],
      deployer.address
    );

    // Check booking details
    const booking = bookingResult.result.expectOk().expectSome();
    assertEquals(booking.data["artisan"], artisan.address);
    assertEquals(booking.data["client"], client.address);
    assertEquals(booking.data["details"], types.utf8("Need help with plumbing"));
    assertEquals(booking.data["status"], types.uint(STATUS_PENDING));
  },
});

Clarinet.test({
  name: "Client can cancel booking within cancellation period",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const artisan = accounts.get("wallet_1")!;
    const client = accounts.get("wallet_2")!;

    // Register artisan
    let block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "register-artisan",
        [
          types.utf8("https://example.com/profile"),
          types.utf8("I am a skilled plumber"),
        ],
        artisan.address
      ),
    ]);

    // Book artisan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "book-artisan",
        [
          types.principal(artisan.address),
          types.utf8("Need help with plumbing"),
          types.uint(1000),
        ],
        client.address
      ),
    ]);

    // Cancel booking
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "cancel-booking",
        [types.uint(1)],
        client.address
      ),
    ]);

    // Check if cancellation was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Get booking details
    const bookingResult = chain.callReadOnlyFn(
      "artisan",
      "get-booking",
      [types.uint(1)],
      deployer.address
    );

    // Check booking status is cancelled
    const booking = bookingResult.result.expectOk().expectSome();
    assertEquals(booking.data["status"], types.uint(STATUS_CANCELLED));
  },
});

Clarinet.test({
  name: "Artisan can complete booking and client can leave feedback",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const artisan = accounts.get("wallet_1")!;
    const client = accounts.get("wallet_2")!;

    // Register artisan
    let block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "register-artisan",
        [
          types.utf8("https://example.com/profile"),
          types.utf8("I am a skilled plumber"),
        ],
        artisan.address
      ),
    ]);

    // Book artisan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "book-artisan",
        [
          types.principal(artisan.address),
          types.utf8("Need help with plumbing"),
          types.uint(1000),
        ],
        client.address
      ),
    ]);

    // Complete booking
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "complete-booking",
        [types.uint(1)],
        artisan.address
      ),
    ]);

    // Check if completion was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Get booking details
    const bookingResult = chain.callReadOnlyFn(
      "artisan",
      "get-booking",
      [types.uint(1)],
      deployer.address
    );

    // Check booking status is completed
    const booking = bookingResult.result.expectOk().expectSome();
    assertEquals(booking.data["status"], types.uint(STATUS_COMPLETED));

    // Submit feedback
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "submit-feedback",
        [
          types.uint(1),
          types.uint(5),
          types.utf8("Excellent service!"),
        ],
        client.address
      ),
    ]);

    // Check if feedback submission was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Get feedback
    const feedbackResult = chain.callReadOnlyFn(
      "artisan",
      "get-feedback",
      [types.principal(artisan.address), types.uint(1)],
      deployer.address
    );

    // Check feedback details
    const feedback = feedbackResult.result.expectOk().expectSome();
    assertEquals(feedback.data["reviewer"], client.address);
    assertEquals(feedback.data["rating"], types.uint(5));
    assertEquals(feedback.data["comment"], types.utf8("Excellent service!"));
  },
});

Clarinet.test({
  name: "Client can create dispute and admin can resolve it",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const artisan = accounts.get("wallet_1")!;
    const client = accounts.get("wallet_2")!;

    // Register artisan
    let block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "register-artisan",
        [
          types.utf8("https://example.com/profile"),
          types.utf8("I am a skilled plumber"),
        ],
        artisan.address
      ),
    ]);

    // Book artisan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "book-artisan",
        [
          types.principal(artisan.address),
          types.utf8("Need help with plumbing"),
          types.uint(1000),
        ],
        client.address
      ),
    ]);

    // Create dispute
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "create-dispute",
        [
          types.uint(1),
          types.utf8("Work not completed properly"),
        ],
        client.address
      ),
    ]);

    // Check if dispute creation was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Get booking details
    const bookingResult = chain.callReadOnlyFn(
      "artisan",
      "get-booking",
      [types.uint(1)],
      deployer.address
    );

    // Check booking status is disputed
    const booking = bookingResult.result.expectOk().expectSome();
    assertEquals(booking.data["status"], types.uint(STATUS_DISPUTED));

    // Resolve dispute in favor of client
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "resolve-dispute",
        [
          types.uint(1),
          types.bool(true), // in favor of client
        ],
        deployer.address
      ),
    ]);

    // Check if dispute resolution was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
  },
});

Clarinet.test({
  name: "Client can set up installment plan and make payments",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const artisan = accounts.get("wallet_1")!;
    const client = accounts.get("wallet_2")!;

    // Register artisan
    let block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "register-artisan",
        [
          types.utf8("https://example.com/profile"),
          types.utf8("I am a skilled plumber"),
        ],
        artisan.address
      ),
    ]);

    // Book artisan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "book-artisan",
        [
          types.principal(artisan.address),
          types.utf8("Need help with plumbing"),
          types.uint(1000),
        ],
        client.address
      ),
    ]);

    // Set up installment plan
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "initiate-installment",
        [
          types.uint(1),
          types.uint(3), // 3 installments
          types.uint(3000), // total amount
          types.uint(144), // interval
        ],
        client.address
      ),
    ]);

    // Check if installment plan setup was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(144));

    // Make a payment
    block = chain.mineBlock([
      Tx.contractCall(
        "artisan",
        "pay-installment",
        [types.uint(1)],
        client.address
      ),
    ]);

    // Check if payment was successful
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1000));
  },
});